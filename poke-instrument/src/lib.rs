use std::{
    arch::x86_64::_rdtsc,
    cell::{Cell, OnceCell},
    fmt::Display,
    num::Wrapping,
    ops,
    sync::atomic::AtomicUsize,
    time::Instant,
};

use thousands::Separable;

#[must_use]
#[derive(Clone)]
pub struct Measurement {
    old_elapsed_root: Cycles,
    old_data_root: u64,
    start_time: AbsoluteCycles,
    data: u64,
    parent_id: Option<usize>,
    self_id: usize,
}

impl Measurement {
    #[inline]
    pub fn new(self_id: usize, data: u64) -> Self {
        let (old_elapsed_root, old_data_root) = ANCHORS.with(|anchors| {
            let anchor = &anchors[self_id];
            (anchor.elapsed_root.get(), anchor.data_root.get())
        });

        let parent_id = ACTIVE.replace(Some(self_id));

        let start_time = AbsoluteCycles::now();

        Self {
            old_elapsed_root,
            old_data_root,
            start_time,
            data,
            parent_id,
            self_id,
        }
    }
}

impl ops::Sub for AbsoluteCycles {
    type Output = Cycles;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output::new(self.0 - rhs.0)
    }
}

impl Drop for Measurement {
    #[inline]
    fn drop(&mut self) {
        let end_time = AbsoluteCycles::now();
        let elapsed = end_time - self.start_time;

        let id = ACTIVE.replace(self.parent_id).unwrap();
        assert_eq!(id, self.self_id);

        ANCHORS.with(|anchors| {
            let anchor = &anchors[self.self_id];
            anchor.elapsed_root.set(self.old_elapsed_root + elapsed);
            anchor.data_root.set(self.old_data_root + self.data);
            cell_update(&anchor.hits_total, |old| old + 1);
            cell_update(&anchor.elapsed_leaf, |old| old + elapsed);

            if let Some(parent_id) = self.parent_id {
                cell_update(&anchors[parent_id].elapsed_leaf, |elapsed_leaf| {
                    elapsed_leaf - elapsed
                });
            }
        });
    }
}

impl AbsoluteCycles {
    #[inline]
    fn now() -> Self {
        Self(unsafe { _rdtsc() })
    }
}

struct Anchor {
    elapsed_leaf: Cell<Cycles>,
    elapsed_root: Cell<Cycles>,
    data_root: Cell<u64>,
    hits_total: Cell<u64>,
}

#[derive(Clone, Copy, derive_more::Add, derive_more::Sub, PartialEq, Eq)]
struct Cycles(Wrapping<u64>);

#[derive(Clone, Copy)]
struct AbsoluteCycles(u64);

impl Cycles {
    #[inline]
    const fn new(cycles: u64) -> Self {
        Self(Wrapping(cycles))
    }

    #[inline]
    fn in_seconds(self, frequency: Frequency) -> f64 {
        self.0 .0 as f64 / frequency.cycles_per_second
    }

    #[inline]
    fn display(self, frequency: Frequency) -> CycleDisplay {
        CycleDisplay {
            cycles: self,
            frequency,
        }
    }
}

#[derive(Clone, Copy)]
struct Frequency {
    cycles_per_second: f64,
}
impl Frequency {
    #[inline]
    fn new(reference_duration: std::time::Duration, cycles_per_duration: Cycles) -> Self {
        let cycles_per_second = cycles_per_duration.0 .0 as f64 / reference_duration.as_secs_f64();
        Self { cycles_per_second }
    }
}

struct CycleDisplay {
    cycles: Cycles,
    frequency: Frequency,
}

impl Display for CycleDisplay {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let seconds = self.cycles.in_seconds(self.frequency);
        seconds.fmt(f)?;
        write!(f, "s")
    }
}

impl Anchor {
    #[inline]
    const fn new() -> Self {
        Self {
            elapsed_leaf: Cell::new(Cycles::new(0)),
            elapsed_root: Cell::new(Cycles::new(0)),
            data_root: Cell::new(0),
            hits_total: Cell::new(0),
        }
    }
}

const MAX_ANCHORS: usize = 128;

thread_local! {
    static ANCHORS: [Anchor; MAX_ANCHORS] = const { [const { Anchor::new()}; MAX_ANCHORS] };
    pub static NAMES: [OnceCell<&'static str>; MAX_ANCHORS] = const { [const { OnceCell::new() }; MAX_ANCHORS] };
    static ACTIVE: Cell<Option<usize>> = const { Cell::new(None) };

}

fn print_statistics(frequency: Frequency) {
    assert!(ACTIVE.get().is_none());
    println!(
        "Frequency: {} MHz",
        frequency.cycles_per_second / 1_000_000.0
    );

    let max_string_length = NAMES
        .with(|array| {
            array
                .iter()
                .filter_map(|val| val.get().map(|name| name.len()))
                .max()
        })
        .unwrap();

    for i in 0..MAX_ANCHORS {
        if let Some(name) = NAMES.with(|array| array[i].get().copied()) {
            ANCHORS.with(|anchors| {
                let anchor = &anchors[i];

                print!(
                    "{:<n$} : {:>5.2}",
                    name,
                    anchor.elapsed_leaf.get().display(frequency),
                    n = max_string_length
                );

                if anchor.elapsed_root.get() != anchor.elapsed_leaf.get() {
                    print!(" / {:>5.2}", anchor.elapsed_root.get().display(frequency));
                } else {
                    print!("         ");
                }

                if anchor.data_root.get() != 0 {
                    print!(
                        " [{:.2} GiB/s]",
                        anchor.data_root.get() as f64
                            / (1024.0 * 1024.0 * 1024.0)
                            / anchor.elapsed_root.get().in_seconds(frequency)
                    )
                }

                if anchor.hits_total.get() != 1 {
                    print!(
                        " {{{}x}}",
                        anchor.hits_total.get().separate_with_underscores()
                    )
                }

                println!();
            });
        }
    }
}

#[inline]
fn cell_update<T: Copy, F>(this: &Cell<T>, f: F) -> T
where
    F: FnOnce(T) -> T,
{
    let old = this.get();
    let new = f(old);
    this.set(new);
    new
}

pub static SCOPE_ID: AtomicUsize = AtomicUsize::new(0);

#[macro_export]
#[cfg(feature = "instrument-basic")]
macro_rules! profile_scope {
    ($name:expr) => {
        $crate::profile_scope!($name, 0)
    };
    ($name:expr, $data:expr) => {
        let _scope = $crate::profile_custom!($name, $data);
    };
}

#[macro_export]
#[cfg(any(feature = "instrument-off", feature = "instrument-main"))]
macro_rules! profile_scope {
    ($name:expr) => {};
    ($name:expr, $data:expr) => {};
}

#[macro_export]
#[cfg(feature = "instrument-basic")]
macro_rules! profile_function {
    () => {
        $crate::profile_function!(0)
    };
    ($data:expr) => {
        $crate::profile_scope!(
            $crate::clean_function_name($crate::current_function_name!()),
            $data
        )
    };
}

#[macro_export]
#[cfg(any(feature = "instrument-off", feature = "instrument-main"))]
macro_rules! profile_function {
    () => {};
    ($data:expr) => {};
}

#[macro_export]
macro_rules! current_function_name {
    () => {{
        fn f() {}
        $crate::type_name_of(f)
    }};
}

#[inline]
pub fn type_name_of<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
}

#[cfg(feature = "instrument-basic")]
#[macro_export]
macro_rules! profile_custom {
    ($name:expr) => {
        $crate::profile_custom!($name, 0)
    };
    ($name:expr, $data:expr) => {
        $crate::__profile_impl!($name, $data)
    };
}

#[cfg(any(feature = "instrument-off", feature = "instrument-main"))]
#[macro_export]
macro_rules! profile_custom {
    ($name:expr) => {};
    ($name:expr, $data:expr) => {};
}

#[cfg(not(feature = "instrument-off"))]
#[macro_export]
macro_rules! __profile_impl {
    ($name:expr, $data:expr) => {{
        static LOCAL_SCOPE_ID: std::sync::LazyLock<usize> = std::sync::LazyLock::new(|| {
            let res = $crate::SCOPE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            $crate::NAMES.with(|array| {
                array[res].get_or_init(|| $name);
            });
            res
        });
        $crate::Measurement::new(*LOCAL_SCOPE_ID, $data)
    }};
}

#[macro_export]
#[cfg(any(feature = "instrument-basic", feature = "instrument-main"))]
macro_rules! profile_main {
    () => {
        let _scope = $crate::MainMeasurement::new($crate::__profile_impl!(
            $crate::clean_function_name($crate::current_function_name!()),
            0
        ));
    };
}

#[macro_export]
#[cfg(feature = "instrument-off")]
macro_rules! profile_main {
    () => {};
}

pub struct MainMeasurement {
    measurement: Option<Measurement>,
    reference_start: Instant,
}

impl MainMeasurement {
    #[inline]
    pub fn new(measurement: Measurement) -> Self {
        let reference_start = Instant::now();
        Self {
            measurement: Some(measurement),
            reference_start,
        }
    }
}

impl Drop for MainMeasurement {
    #[inline]
    fn drop(&mut self) {
        let measurement = self.measurement.take().unwrap();
        let root_id = measurement.self_id;

        let reference_end = Instant::now();
        drop(measurement);

        let reference_duration = reference_end - self.reference_start;
        let cycles_per_duration = ANCHORS.with(|anchors| anchors[root_id].elapsed_root.get());
        let frequency = Frequency::new(reference_duration, cycles_per_duration);

        print_statistics(frequency);
    }
}

// The macro defines 'f()' at the place where macro is called.
// This code is typically located at the place of call and two closures deep.
// Strip away this useless suffix.
pub(crate) const USELESS_SCOPE_NAME_SUFFIX: &str =
    "::LOCAL_SCOPE_ID::{{closure}}::{{closure}}::{{closure}}::f";
pub(crate) const USELESS_CLOSURE_SUFFIX: &str = "::{{closure}}";

#[inline]
pub fn clean_function_name(name: &'static str) -> &'static str {
    let Some(name) = name.strip_suffix(USELESS_SCOPE_NAME_SUFFIX) else {
        // Probably the user registered a user scope name.
        return name;
    };
    // Remove any additional trailing suffixes
    shorten_rust_function_name(name.trim_end_matches(USELESS_CLOSURE_SUFFIX))
}

/// Shorten a rust function name by removing the leading parts of module paths.
///
/// While the puffin profiling macros takes care of this internally, this function can be
/// useful for those registering custom scopes for rust functions.
///
/// # Example
/// ```
/// use puffin::shorten_rust_function_name;
///
/// assert_eq!(shorten_rust_function_name("foo::bar::baz::function_name"), "baz::function_name");
/// assert_eq!(shorten_rust_function_name("<some::ConcreteType as some::Trait>::function_name"), "<ConcreteType as Trait>::function_name");
/// ```
#[inline]
pub fn shorten_rust_function_name(name: &'static str) -> &'static str {
    // "foo::bar::baz" -> "baz"
    #[expect(dead_code)]
    fn last_part(name: &str) -> &str {
        if let Some(colon) = name.rfind("::") {
            &name[colon + 2..]
        } else {
            name
        }
    }

    // look for:  <some::ConcreteType as some::Trait>::function_name
    if let Some(end_caret) = name.rfind('>') {
        if let Some(trait_as) = name.rfind(" as ") {
            if trait_as < end_caret {
                // let concrete_name = if let Some(start_caret) = name[..trait_as].rfind('<') {
                //     &name[start_caret + 1..trait_as]
                // } else {
                //     name
                // };

                // let trait_name = &name[trait_as + 4..end_caret];

                // let concrete_name = last_part(concrete_name);
                // let trait_name = last_part(trait_name);

                // let dubcolon_function_name = &name[end_caret + 1..];
                panic!()
                // return format!("<{concrete_name} as {trait_name}>{dubcolon_function_name}");
            }
        }
    }

    if let Some(colon) = name.rfind("::") {
        if let Some(colon) = name[..colon].rfind("::") {
            // "foo::bar::baz::function_name" -> "baz::function_name"
            &name[colon + 2..]
        } else {
            // "foo::function_name" -> "foo::function_name"
            name
        }
    } else {
        name
    }
}

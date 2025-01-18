use anyhow::anyhow;
use anyhow::Context;
use poke_part2::haversine::reference_haversine;
use rand::distributions::Uniform;
use rand::prelude::Distribution;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaCha8Rng;
use serde::Serialize;
use std::env;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::str::FromStr;

enum Mode {
    Uniform,
    Cluser,
}

impl FromStr for Mode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "uniform" => Ok(Self::Uniform),
            "cluster" => Ok(Self::Cluser),
            mode => Err(anyhow!("unknown mode '{mode}'")),
        }
    }
}

#[derive(Serialize)]
struct Instances {
    pairs: Vec<Instance>,
}

#[derive(Serialize)]
struct Instance {
    x0: f64,
    y0: f64,
    x1: f64,
    y1: f64,
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();

    let mode: Mode = args.get(1).context("please provide a mode")?.parse()?;

    let seed = args.get(2).context("please provide a seed")?.parse()?;

    let amount = args.get(3).context("please provide an amount")?.parse()?;

    let cluster_size = amount / 16;

    let mut rng = ChaCha8Rng::seed_from_u64(seed);

    let earth_radius = 6372.8;

    let max_x_range = Uniform::from(-180.0..180.0);
    let max_y_range = Uniform::from(-90.0..90.0);

    let mut x_range = max_x_range;
    let mut y_range = max_y_range;

    let mut pairs = Vec::with_capacity(amount);
    let mut results = Vec::with_capacity(amount);
    for i in 0..amount {
        if matches!(mode, Mode::Cluser) && i % cluster_size == 0 {
            let x_range_a = max_x_range.sample(&mut rng);
            let x_range_b = max_x_range.sample(&mut rng);
            x_range = Uniform::from(f64::min(x_range_a, x_range_b)..f64::max(x_range_a, x_range_b));

            let y_range_a = max_y_range.sample(&mut rng);
            let y_range_b = max_y_range.sample(&mut rng);
            y_range = Uniform::from(f64::min(y_range_a, y_range_b)..f64::max(y_range_a, y_range_b));
        }

        let x0 = x_range.sample(&mut rng);
        let x1 = x_range.sample(&mut rng);
        let y0 = y_range.sample(&mut rng);
        let y1 = y_range.sample(&mut rng);

        let dist = reference_haversine(x0, y0, x1, y1, earth_radius);

        pairs.push(Instance { x0, y0, x1, y1 });
        results.push(dist);
    }

    let average = results.iter().sum::<f64>() / amount as f64;
    println!("Average: {}", average);

    let instances = Instances { pairs };

    create_dir_all("out")?;

    let file = File::create("out/instances.json")?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, &instances)?;
    writer.flush()?;

    let file = File::create("out/results")?;
    let mut writer = BufWriter::new(file);
    for result in results {
        writer.write_all(&result.to_ne_bytes())?;
    }
    writer.write_all(&average.to_ne_bytes())?;
    writer.flush()?;

    Ok(())
}

use anyhow::Context;
use poke_part1::i8086::{simulate, LogContext, SimulateLogOptions};
use std::io::Write;
use std::{
    env,
    io::{sink, stdout},
};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let input_file = args.get(1).context("please provide an input file")?;

    let mut log_context = LogContext::new(Box::new(sink()), SimulateLogOptions::new(true, None));

    let memory = simulate(input_file, &mut log_context)?;

    stdout().write_all(&memory)?;

    Ok(())
}

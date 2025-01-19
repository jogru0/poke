use std::fs::read;

use anyhow::Ok;
use itertools::Itertools;
use poke_part2::{
    haversine::reference_haversine, haversine_json::to_haversine_instances, json::load_json_file,
};

fn main() -> anyhow::Result<()> {
    let json_file_name = "out/instances.json";
    let verification_file_name = "out/results";

    let json = load_json_file(json_file_name)?;

    let instances = to_haversine_instances(&json)?;

    let earth_radius = 6372.8;

    let amount = instances.len();

    let results = instances
        .into_iter()
        .map(|instance| {
            reference_haversine(
                instance.x0,
                instance.y0,
                instance.x1,
                instance.y1,
                earth_radius,
            )
        })
        .collect_vec();

    let sum: f64 = results.iter().sum();
    let average = sum / amount as f64;

    let verification_bytes = read(verification_file_name)?;
    const F64_SIZE: usize = size_of::<f64>();
    assert_eq!(verification_bytes.len() % F64_SIZE, 0);

    let mut verification_results = (0..verification_bytes.len() / F64_SIZE)
        .map(|i| {
            let bytes = verification_bytes[F64_SIZE * i..F64_SIZE * (i + 1)].try_into()?;
            Ok(f64::from_ne_bytes(bytes))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let verification_average = verification_results.pop().unwrap();

    assert_eq!(results.len(), verification_results.len());
    assert_eq!(average, verification_average);
    assert_eq!(results, verification_results);

    println!("average: {average}",);

    Ok(())
}

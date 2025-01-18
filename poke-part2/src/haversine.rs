fn square(x: f64) -> f64 {
    x * x
}

pub fn reference_haversine(x0: f64, y0: f64, x1: f64, y1: f64, earth_radius: f64) -> f64 {
    let lat1 = y0;
    let lat2 = y1;
    let lon1 = x0;
    let lon2 = x1;

    let dlat = (lat2 - lat1).to_radians();
    let dlon = (lon2 - lon1).to_radians();
    let lat1 = (lat1).to_radians();
    let lat2 = (lat2).to_radians();

    let a = square(f64::sin(dlat / 2.0))
        + f64::cos(lat1) * f64::cos(lat2) * square(f64::sin(dlon / 2.0));
    let c = 2.0 * f64::asin(a.sqrt());

    earth_radius * c
}

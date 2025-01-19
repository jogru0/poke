use anyhow::bail;

use crate::json::{JsonList, JsonNumber, JsonObject, JsonValue};

pub struct HaversineInstance {
    pub x0: f64,
    pub y0: f64,
    pub x1: f64,
    pub y1: f64,
}

fn to_haversine_instance(json: &JsonValue) -> anyhow::Result<HaversineInstance> {
    let JsonValue::Object(JsonObject(dict)) = json else {
        bail!("");
    };

    let Some(&JsonValue::Number(JsonNumber(x0))) = dict.get("x0") else {
        bail!("")
    };

    let Some(&JsonValue::Number(JsonNumber(y0))) = dict.get("y0") else {
        bail!("")
    };

    let Some(&JsonValue::Number(JsonNumber(x1))) = dict.get("x1") else {
        bail!("")
    };

    let Some(&JsonValue::Number(JsonNumber(y1))) = dict.get("y1") else {
        bail!("")
    };

    Ok(HaversineInstance { x0, y0, x1, y1 })
}

pub fn to_haversine_instances(json: &JsonValue) -> anyhow::Result<Vec<HaversineInstance>> {
    let JsonValue::Object(JsonObject(dict)) = json else {
        bail!("");
    };

    let Some(JsonValue::List(JsonList(entries))) = dict.get("pairs") else {
        bail!("")
    };

    entries.iter().map(to_haversine_instance).collect()
}

use anyhow::Result;

pub async fn day2(_input: String) -> Result<(String, String)> {
    Err(anyhow::anyhow!("Test"))?;
    Ok(("Part1".to_string(), "Part2".to_string()))
}

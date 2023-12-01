use std::io::ErrorKind;

use anyhow::Result;
use reqwest::Client;

const YEAR: u32 = 2023;
const BASE_URL: &'static str = "https://adventofcode.com";

pub struct Aoc {
    session: String,
    client: Client,
}

impl Aoc {
    pub fn new(session: impl AsRef<str>) -> Self {
        if let Err(e) = std::fs::create_dir_all("./inputs") {
            println!("Warning: Couldn't create inputs directory, this means no caching will work and the api will be spammed, which is not good. Error: {e}");
        }
        Self {
            session: session.as_ref().into(),
            client: Client::new(),
        }
    }

    async fn fetch_input(&self, day: u32) -> Result<String, reqwest::Error> {
        self.client
            .get(format!("{BASE_URL}/{YEAR}/day/{day}/input"))
            .header("Cookie", format!("session={}", self.session))
            .send()
            .await?
            .text()
            .await
    }

    async fn fetch_and_cache(&self, day: u32) -> Result<String> {
        let input = self.fetch_input(day).await?;
        // Technically failing to cache isn't a problem for the runtime, so no need to return an
        // error, simply warn the user since this isn't ideal.
        if let Err(e) = std::fs::write(format!("./inputs/{day}"), &input) {
            eprintln!("{e}");
            eprintln!("Warning: Couldn't save input file, this can lead to spamming of the api and should be avoided to not get banned.");
        }
        Ok(input)
    }

    pub async fn get_input(&self, day: u32) -> Result<String> {
        match std::fs::read_to_string(format!("./inputs/{day}")) {
            Ok(input) => Ok(input),
            Err(err) => if err.kind() == ErrorKind::NotFound {
                Ok(self.fetch_and_cache(day).await?)
            } else {
                Err(err.into())
            }
        }
    }
}

use serde::de::{self, Visitor};
use serde::Deserialize;
use std::cmp::Ordering;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

#[macro_export]
macro_rules! free_slices_of_day {
    ($d:ident, $x:ident) => {{
        let persons = $d
            .persons
            .iter()
            .map(|person| &person.free_times.$x)
            .collect::<Vec<&Vec<TimeSlice>>>();

        let mut results = vec![TimeSlice::new(
            Time::new(0, 0).unwrap(),
            Time::new(23, 59).unwrap(),
        )];
        for person in persons {
            let mut new_result = Vec::new();
            for slice in person {
                for res_slice in &results {
                    let intersection = slice.intersection_with(&res_slice);
                    if intersection.len() >= $d.meeting_length {
                        new_result.push(intersection);
                    }
                }
            }
            results = new_result;
        }
        results
    }};
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Time {
    hour: u16,
    min: u16,
}

impl FromStr for Time {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let raw_parts: Vec<&str> = s.split(':').collect();
        Ok(Time::new(raw_parts[0].parse()?, raw_parts[1].parse()?).unwrap())
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:0>2}:{:0>2}", self.hour, self.min)
    }
}

impl<'de> de::Deserialize<'de> for Time {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_str(TimeVisitor)
    }
}

struct TimeVisitor;

impl<'de> Visitor<'de> for TimeVisitor {
    type Value = Time;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct Time")
    }

    fn visit_str<E>(self, value: &str) -> Result<Time, E>
    where
        E: de::Error,
    {
        value.parse().map_err(E::custom)
    }
}

#[derive(Debug)]
pub enum TimeError {
    ParseError,
}

impl Time {
    pub fn new(hour: u16, min: u16) -> Result<Self, TimeError> {
        if min > 59 || hour > 59 {
            Err(TimeError::ParseError)
        } else {
            Ok(Self { min, hour })
        }
    }

    pub fn to_minutes(self: &Self) -> u16 {
        self.hour.wrapping_mul(60).wrapping_add(self.min)
    }

    pub fn max_with<'a>(self: &'a Self, other: &'a Self) -> &'a Self {
        match self.cmp(other) {
            Ordering::Equal | Ordering::Greater => self,
            Ordering::Less => other,
        }
    }

    pub fn min_with<'a>(self: &'a Self, other: &'a Self) -> &'a Self {
        match self.cmp(other) {
            Ordering::Equal | Ordering::Greater => other,
            Ordering::Less => self,
        }
    }
}

impl Ord for Time {
    fn cmp(self: &Self, other: &Self) -> Ordering {
        self.to_minutes().cmp(&other.to_minutes())
    }
}

impl PartialOrd for Time {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct TimeSlice(Time, Time);

impl TimeSlice {
    pub fn new(start: Time, end: Time) -> Self {
        Self(start, end)
    }

    pub fn intersection_with(self: &Self, other: &Self) -> Self {
        if (self.0 < other.0 && self.1 < other.0) || (other.0 < self.0 && other.1 < self.0) {
            let midnight = (Time::new(0, 0)).unwrap();
            Self(midnight, midnight)
        } else {
            let start = self.0.max_with(&other.0).clone();
            let end = self.1.min_with(&other.1).clone();
            Self(start, end)
        }
    }

    pub fn is_empty(self: &Self) -> bool {
        self.0 == self.1
    }

    pub fn len(self: &Self) -> u16 {
        let start_min = self.0.hour * 60 + self.0.min;
        let end_min = self.1.hour * 60 + self.1.min;
        end_min - start_min
    }
}

impl fmt::Display for TimeSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.0, self.1)
    }
}

#[derive(Deserialize, Debug)]
pub struct PersonSchedule {
    pub name: String,
    pub free_times: Schedule,
}

#[derive(Deserialize, Debug)]
pub struct Schedule {
    pub sat: Vec<TimeSlice>,
    pub sun: Vec<TimeSlice>,
    pub mon: Vec<TimeSlice>,
    pub tue: Vec<TimeSlice>,
    pub wed: Vec<TimeSlice>,
    pub thu: Vec<TimeSlice>,
    pub fri: Vec<TimeSlice>,
}

impl fmt::Display for Schedule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sat: ")?;
        for i in &self.sat {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Sun: ")?;
        for i in &self.sun {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Mon: ")?;
        for i in &self.mon {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Tue: ")?;
        for i in &self.tue {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Wed: ")?;
        for i in &self.wed {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Thu: ")?;
        for i in &self.thu {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")?;

        write!(f, "Fri: ")?;
        for i in &self.fri {
            if !i.is_empty() {
                write!(f, "{}, ", i)?;
            }
        }
        write!(f, "\n")
    }
}

#[derive(Deserialize, Debug)]
pub struct Data {
    pub meeting_length: u16,
    #[serde(rename = "Persons")]
    pub persons: Vec<PersonSchedule>,
}

impl Data {
    pub fn find_free_slices(self: &Self) -> Schedule {
        Schedule {
            sat: free_slices_of_day!(self, sat),
            sun: free_slices_of_day!(self, sun),
            mon: free_slices_of_day!(self, mon),
            tue: free_slices_of_day!(self, tue),
            wed: free_slices_of_day!(self, wed),
            thu: free_slices_of_day!(self, thu),
            fri: free_slices_of_day!(self, fri),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_intersection_1() {
        let a = TimeSlice::new(Time::new(8, 0).unwrap(), Time::new(9, 0).unwrap());
        let b = TimeSlice::new(Time::new(7, 0).unwrap(), Time::new(10, 0).unwrap());
        assert_eq!(a.intersection_with(&b), a)
    }

    #[test]
    fn test_intersection_2() {
        let a = TimeSlice::new(Time::new(7, 30).unwrap(), Time::new(10, 5).unwrap());
        let b = TimeSlice::new(Time::new(8, 9).unwrap(), Time::new(9, 30).unwrap());
        assert_eq!(a.intersection_with(&b), b)
    }

    #[test]
    fn test_intersection_3() {
        let a = TimeSlice::new(Time::new(7, 30).unwrap(), Time::new(10, 5).unwrap());
        let b = TimeSlice::new(Time::new(11, 9).unwrap(), Time::new(19, 30).unwrap());
        assert!(a.intersection_with(&b).is_empty())
    }

    #[test]
    fn test_intersection_4() {
        let a = TimeSlice::new(Time::new(11, 9).unwrap(), Time::new(19, 30).unwrap());
        let b = TimeSlice::new(Time::new(7, 30).unwrap(), Time::new(10, 5).unwrap());
        assert!(a.intersection_with(&b).is_empty())
    }

    #[test]
    fn test_intersection_5() {
        let a = TimeSlice::new(Time::new(7, 9).unwrap(), Time::new(9, 30).unwrap());
        let b = TimeSlice::new(Time::new(8, 30).unwrap(), Time::new(10, 5).unwrap());
        let result = TimeSlice::new(Time::new(8, 30).unwrap(), Time::new(9, 30).unwrap());
        assert_eq!(a.intersection_with(&b), result)
    }

    #[test]
    fn test_intersection_6() {
        let a = TimeSlice::new(Time::new(8, 30).unwrap(), Time::new(11, 5).unwrap());
        let b = TimeSlice::new(Time::new(7, 9).unwrap(), Time::new(9, 30).unwrap());
        let result = TimeSlice::new(Time::new(8, 30).unwrap(), Time::new(9, 30).unwrap());
        assert_eq!(a.intersection_with(&b), result)
    }

    #[test]
    fn test_intersection_7() {
        let a = TimeSlice::new(Time::new(8, 30).unwrap(), Time::new(11, 5).unwrap());
        assert_eq!(a.intersection_with(&a), a)
    }
}

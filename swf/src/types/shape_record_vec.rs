use crate::types::{ShapeRecord, StyleChangeData, StraightEdgeData, CurvedEdgeData};

#[derive(Clone, Copy, Debug)]
pub enum ShapeRecordRef<'a> {
    StyleChange(&'a StyleChangeData),
    StraightEdge(&'a StraightEdgeData),
    CurvedEdge(&'a CurvedEdgeData),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ShapeRecordDiscriminant {
    StyleChange,
    StraightEdge,
    CurvedEdge,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapeRecordVec {
    discriminants: Vec<ShapeRecordDiscriminant>,
    style_data: Vec<StyleChangeData>,
    straight_data: Vec<StraightEdgeData>,
    curved_data: Vec<CurvedEdgeData>,
}

impl ShapeRecordVec {
    pub fn new() -> Self {
        Self {
            discriminants: Vec::new(),
            style_data: Vec::new(),
            straight_data: Vec::new(),
            curved_data: Vec::new(),
        }
    }

    pub fn push(&mut self, value: ShapeRecord) {
        match value {
            ShapeRecord::StyleChange(x) => {
                self.discriminants.push(ShapeRecordDiscriminant::StyleChange);
                self.style_data.push(x);
            }
            ShapeRecord::StraightEdge(x) => {
                self.discriminants.push(ShapeRecordDiscriminant::StraightEdge);
                self.straight_data.push(x);
            }
            ShapeRecord::CurvedEdge(x) => {
                self.discriminants.push(ShapeRecordDiscriminant::CurvedEdge);
                self.curved_data.push(x);
            }
        }
    }

    pub fn iter<'a>(&'a self) -> ShapeRecordVecIter<'a> {
        ShapeRecordVecIter {
            disc_iter: self.discriminants.iter(),
            style_iter: self.style_data.iter(),
            straight_iter: self.straight_data.iter(),
            curved_iter: self.curved_data.iter(),
        }
    }
}

impl<'a> IntoIterator for &'a ShapeRecordVec {
    type Item = ShapeRecordRef<'a>;
    type IntoIter = ShapeRecordVecIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct ShapeRecordVecIter<'a> {
    disc_iter: std::slice::Iter<'a, ShapeRecordDiscriminant>,
    style_iter: std::slice::Iter<'a, StyleChangeData>,
    straight_iter: std::slice::Iter<'a, StraightEdgeData>,
    curved_iter: std::slice::Iter<'a, CurvedEdgeData>,
}

impl<'a> Iterator for ShapeRecordVecIter<'a> {
    type Item = ShapeRecordRef<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let discriminant = match self.disc_iter.next() {
            Some(x) => x,
            None => return None
        };
        match discriminant {
            ShapeRecordDiscriminant::StyleChange => {
                let val = self.style_iter.next();
                if val.is_none() { unsafe { std::hint::unreachable_unchecked(); } }
                Some(ShapeRecordRef::StyleChange(val.unwrap()))
            },
            ShapeRecordDiscriminant::StraightEdge => {
                let val = self.straight_iter.next();
                if val.is_none() { unsafe { std::hint::unreachable_unchecked(); } }
                Some(ShapeRecordRef::StraightEdge(val.unwrap()))
            },
            ShapeRecordDiscriminant::CurvedEdge => {
                let val = self.curved_iter.next();
                if val.is_none() { unsafe { std::hint::unreachable_unchecked(); } }
                Some(ShapeRecordRef::CurvedEdge(val.unwrap()))
            },
        }
    }
}


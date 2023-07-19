use crate::Fixed16;
use bitflags::bitflags;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BlurFilter {
    pub blur_x: Fixed16,
    pub blur_y: Fixed16,
    pub flags: BlurFilterFlags,
}

impl BlurFilter {
    #[inline]
    pub fn num_passes(&self) -> u8 {
        (self.flags & BlurFilterFlags::PASSES).bits() >> 3
    }

    pub fn scale(&mut self, x: f32, y: f32) {
        self.blur_x *= Fixed16::from_f32(x);
        self.blur_y *= Fixed16::from_f32(y);
    }

    pub fn impotent(&self) -> bool {
        self.blur_x == Fixed16::ZERO && self.blur_y == Fixed16::ZERO
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct BlurFilterFlags: u8 {
        const PASSES = 0b11111 << 3;
    }
}

impl BlurFilterFlags {
    #[inline]
    pub fn from_passes(num_passes: u8) -> Self {
        let flags = Self::from_bits_retain(num_passes << 3);
        debug_assert_eq!(flags & Self::PASSES, flags);
        flags
    }
}

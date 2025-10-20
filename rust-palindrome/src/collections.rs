#[derive(Default)]
pub struct FactorBuf {
    buf: [u32; 4], // [x0, y0, x1, y1]
    len: usize,    // 0, 2, or 4 (number of valid u32s)
}

impl FactorBuf {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            buf: [0; 4],
            len: 0,
        }
    }

    #[inline(always)]
    pub fn push_pair(&mut self, x: u32, y: u32) {
        debug_assert!(self.len <= 4);

        if self.len == 0 {
            let out = self.as_mut_ptr();
            unsafe {
                core::ptr::write(out.add(0), x);
                core::ptr::write(out.add(1), y);
            }
            self.len = 2;
        } else if self.len == 2 {
            let out = self.as_mut_ptr();
            unsafe {
                core::ptr::write(out.add(2), x);
                core::ptr::write(out.add(3), y);
            }
            self.len = 4;
        }
    }

    #[inline(always)]
    pub fn with_product(self, product: u32) -> PalOut {
        PalOut {
            product,
            pairs: self.buf,
        }
    }

    #[inline(always)]
    pub fn as_mut_ptr(&mut self) -> *mut u32 {
        self.buf.as_mut_ptr()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn set_len(&mut self, len: usize) {
        self.len = len;
    }
}

pub struct PalOut {
    pub product: u32,
    pub pairs: [u32; 4],
}

impl PalOut {
    #[inline(always)]
    pub fn consume(self) -> u64 {
        (self.product as u64)
            + (self.pairs[0] as u64)
            + (self.pairs[1] as u64)
            + (self.pairs[2] as u64)
            + (self.pairs[3] as u64)
    }
}

#[repr(align(64))]
pub(crate) struct AlignedBuf<const CAP: usize>(pub(crate) [u32; CAP]);

#[repr(C)]
pub struct Scratch<const CAP: usize> {
    pub(crate) buf: AlignedBuf<CAP>, // starts at offset 0, 64 aligned.
    pub(crate) head: usize,          // at offset 64
    pub(crate) len: usize,           // at offset 72
}

impl<const CAP: usize> Scratch<CAP> {
    pub const MASK: usize = CAP - 1;

    #[inline(always)]
    pub fn new() -> Self {
        Self {
            buf: AlignedBuf([0; CAP]),
            head: 0,
            len: 0,
        }
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.head = 0;
        self.len = 0;
    }

    #[inline(always)]
    pub fn free(&self) -> usize {
        CAP - self.len
    }

    #[inline(always)]
    pub fn tail_start(&self) -> usize {
        debug_assert!(CAP.is_power_of_two());
        (self.head + self.len) & Self::MASK
    }

    #[inline(always)]
    pub fn commit_batch(&mut self, added: usize) {
        debug_assert!(self.len + added <= CAP);
        self.len += added;
    }
}

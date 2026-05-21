#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct SignatureFingerprinter;

impl SignatureFingerprinter {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn stable_text(&self, text: &str) -> u64 {
        const OFFSET: u64 = 0xcbf29ce484222325;
        const PRIME: u64 = 0x100000001b3;
        let mut hash = OFFSET;
        for byte in text.as_bytes() {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(PRIME);
        }
        hash
    }
}

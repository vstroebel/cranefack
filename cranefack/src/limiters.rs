pub enum LimiterResult {
    /// Continue executing
    Continue,
    /// Halt execution and return [`LimiterTriggered`][crate::errors::RuntimeError::LimiterTriggered]
    Halt,
}

/// Generic interface for determining whether to continue or stop execution.
pub trait Limiter {
    fn execute_op(&mut self) -> LimiterResult;
}

/// Always returns [`Continue`][LimiterResult::Continue].
pub struct Unlimited;

impl Limiter for Unlimited {
    fn execute_op(&mut self) -> LimiterResult {
        LimiterResult::Continue
    }
}

/// Limit execution to within the configured number of cycles.
pub struct CycleLimiter(usize);

impl CycleLimiter {
    pub fn new(max_cycles: usize) -> Self {
        CycleLimiter(max_cycles)
    }
}

impl Limiter for CycleLimiter {
    fn execute_op(&mut self) -> LimiterResult {
        self.0 = self.0.saturating_sub(1);
        if self.0 == 0 {
            LimiterResult::Halt
        } else {
            LimiterResult::Continue
        }
    }
}

/// Limits execution to within a given [`Duration`](std::time::Duration) of time.
pub struct TimeLimiter {
    max_duration: std::time::Duration,
    started_at: Option<std::time::Instant>,
}

impl TimeLimiter {
    pub fn new(max_duration: std::time::Duration) -> Self {
        TimeLimiter {
            max_duration,
            started_at: None,
        }
    }
}

impl Limiter for TimeLimiter {
    fn execute_op(&mut self) -> LimiterResult {
        let started_at = self.started_at.get_or_insert(std::time::Instant::now());
        if started_at.elapsed() > self.max_duration {
            LimiterResult::Halt
        } else {
            LimiterResult::Continue
        }
    }
}

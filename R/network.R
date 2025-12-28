# R/network.R
#
# ✅ REFACTORING COMPLETE - This file has been split into focused modules:
#
# Split Date: 2025-12-27
# Original Size: 628 lines
# New Structure: 3 focused modules
#
# Module Locations:
# - R/network-config.R (~140 lines) - Configuration, rate limiting, error classification
#   Environments: .network_options, .rate_limiter
#   Functions: .apply_rate_limit(), .is_retryable_error()
#
# - R/network-retry.R (~193 lines) - Request retry logic and failure tracking
#   Functions: rb_request_with_retry(), rb_network_failures()
#
# - R/network-batch.R (~297 lines) - Checkpointing and batch processing
#   Functions: rb_checkpoints(), rb_clear_checkpoints(), rb_batch_process()
#   Helpers: .get_checkpoint_dir(), .save_checkpoint(), .load_checkpoint()
#
# This file is kept as a placeholder for documentation purposes.
# All functions have been moved to their respective modules.
# See CLAUDE.md for refactoring progress and rationale.

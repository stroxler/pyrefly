/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::backtrace::Backtrace;

use tracing::error;

pub fn exit_on_panic() {
    std::panic::set_hook(Box::new(move |info| {
        error!(
            "Thread panicked, shutting down: {info}\nBacktrace:\n{}",
            Backtrace::force_capture()
        );
        std::process::exit(1);
    }));
}

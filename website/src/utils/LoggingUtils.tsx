/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

export enum LoggingEvent {
    CLICK = 'click',
    HOVER = 'hover',
}

export function log(
    event: LoggingEvent,
    eventParams: Gtag.ControlParams | Gtag.EventParams | Gtag.CustomParams
) {
    if (typeof window !== 'undefined' && window.gtag) {
        window.gtag('event', event, eventParams);
    }
}

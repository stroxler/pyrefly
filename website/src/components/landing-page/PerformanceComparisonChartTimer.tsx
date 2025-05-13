/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React, { useState, useEffect } from 'react';

interface TimerProps {
    targetSeconds: number;
}

// This ensures number of decimal space to be 1. If we ever update this and ends up with more than
// 1 decimal space, make sure to update minWidth of styles.duration in PerformanceComparisonChart.tsx
const UPDATE_FREQUENCY_MS = 100;

const PerformanceComparisonChartTimer: React.FC<TimerProps> = ({
    targetSeconds,
}) => {
    const [seconds, setSeconds] = useState(0.0);

    useEffect(() => {
        if (seconds <= targetSeconds) {
            const interval = setInterval(() => {
                setSeconds((prevSeconds) => {
                    const amountToIncrement = UPDATE_FREQUENCY_MS / 1000;
                    const roundingModifier = 1000 / UPDATE_FREQUENCY_MS;
                    return (
                        Math.round(
                            (prevSeconds + amountToIncrement) * roundingModifier
                        ) / roundingModifier
                    );
                });
            }, UPDATE_FREQUENCY_MS);

            return () => clearInterval(interval);
        }
    }, [seconds, targetSeconds]);

    const numDecimalSpaces = Math.ceil(
        Math.log(1000 / UPDATE_FREQUENCY_MS) / Math.log(10)
    );

    return <strong>{`${seconds.toFixed(numDecimalSpaces)}s`}</strong>;
};

export default PerformanceComparisonChartTimer;

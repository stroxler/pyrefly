/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import * as stylex from '@stylexjs/stylex';
import PerformanceComparisonChart from './PerformanceComparisonChart';
import PerformanceComparisonDescription from './PerformanceComparisonDescription';

export default function PerformanceComparisonChartSection(): React.ReactElement {
    return (
        <div {...stylex.props(styles.body)}>
            <div {...stylex.props(styles.chartContainer)}>
                <PerformanceComparisonDescription />
                <PerformanceComparisonChart />
            </div>
        </div>
    );
}

const styles = stylex.create({
    body: {
        border: '1px solid',
        borderRadius: '4px',
        backgroundColor: 'var(--color-text)',
        marginVertical: '2rem',
        paddingVertical: '2rem',
        paddinghorizontal: '1rem',
    },
    buttonRow: {
        display: 'flex',
        justifyContent: 'center',
        gap: '1rem',
        marginBottom: '2rem',
    },
    chartContainer: { paddingHorizontal: '1rem' },
});

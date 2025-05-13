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
import PerformanceComparisonButton from './PerformanceComparisonButton';
import { Project, ProjectValue } from './PerformanceComparisonTypes';
import { useState } from 'react';

export default function PerformanceComparisonChartSection(): React.ReactElement {
    const [selectedProject, setSelectedProject] = useState<ProjectValue>(
        Project.PYTORCH
    );

    return (
        <div {...stylex.props(styles.body)}>
            <div {...stylex.props(styles.buttonRow)}>
                <PerformanceComparisonButton
                    project={Project.PYTORCH}
                    selectedProject={selectedProject}
                    setSelectedProject={setSelectedProject}
                />
                <PerformanceComparisonButton
                    project={Project.INSTAGRAM}
                    selectedProject={selectedProject}
                    setSelectedProject={setSelectedProject}
                />
            </div>
            <div {...stylex.props(styles.chartContainer)}>
                <PerformanceComparisonDescription />
                <PerformanceComparisonChart project={selectedProject} />
            </div>
        </div>
    );
}

const styles = stylex.create({
    body: {
        padding: '1.75rem',
        background: 'var(--color-background)',
        backdropFilter: 'blur(10px)',
        borderRadius: '8px',
        border: '1px solid rgba(255, 255, 255, 0.1)',
        boxShadow:
            '0 4px 6px var(--color-shadow), 0 1px 3px var(--color-shadow)',
        marginVertical: '2rem',
        paddingVertical: '2rem',
        paddinghorizontal: '1rem',
        transition: 'all 0.3s ease',
        ':hover': {
            boxShadow:
                '0 10px 20px var(--color-shadow-hovered), 0 3px 6px var(--color-shadow)',
        },
    },
    buttonRow: {
        display: 'flex',
        justifyContent: 'center',
        gap: '1rem',
        marginBottom: '2rem',
    },
    chartContainer: { paddingHorizontal: '1rem' },
});

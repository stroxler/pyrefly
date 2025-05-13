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
import { landingPageCardStyles } from './landingPageCardStyles';

export default function PerformanceComparisonChartSection(): React.ReactElement {
    const [selectedProject, setSelectedProject] = useState<ProjectValue>(
        Project.PYTORCH
    );

    return (
        <div {...stylex.props(landingPageCardStyles.card, styles.body)}>
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
                <PerformanceComparisonDescription project={selectedProject} />
                <PerformanceComparisonChart project={selectedProject} />
            </div>
        </div>
    );
}

const styles = stylex.create({
    body: {
        padding: '1.75rem',
        ':hover': {
            background: 'var(--color-landing-page-card-background)',
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

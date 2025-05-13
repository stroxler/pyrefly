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
import {
    Project,
    TypeChecker,
    type TypeCheckerValue,
} from './PerformanceComparisonTypes';
import typography from './typography';

interface TypecheckerTooltipProps {
    typechecker: TypeCheckerValue;
    project: Project;
}

export default function TypecheckerTooltip({
    typechecker,
    project,
}: TypecheckerTooltipProps): React.ReactElement {
    return (
        <div {...stylex.props(styles.tooltipContainer)}>
            <span
                {...stylex.props(typography.p, styles.infoIcon)}
                onMouseEnter={(e) => {
                    const tooltip = e.currentTarget.nextElementSibling;
                    if (tooltip) {
                        (tooltip as HTMLElement).style.visibility = 'visible';
                        (tooltip as HTMLElement).style.opacity = '1';
                    }
                }}
                onMouseLeave={(e) => {
                    const tooltip = e.currentTarget.nextElementSibling;
                    if (tooltip) {
                        (tooltip as HTMLElement).style.visibility = 'hidden';
                        (tooltip as HTMLElement).style.opacity = '0';
                    }
                }}
            >
                ⓘ
            </span>
            <span {...stylex.props(styles.tooltip)}>
                {getTypecheckerInfo(typechecker, project)}
            </span>
        </div>
    );
}

export function getTypecheckerInfo(
    typechecker: TypeCheckerValue,
    project: Project
): React.ReactNode {
    switch (typechecker) {
        case TypeChecker.PYREFLY:
            // Include branching for Pyrefly since we only run Pyrefly on more than one project
            switch (project) {
                case Project.INSTAGRAM:
                    return (
                        <>
                            <strong>Command: 'Pyrefly check'</strong>
                            <br />
                            Pyrefly uses as many threads as possible, which was
                            "INSERT NUMBER HERE" on a 200 core machine.
                        </>
                    );
                case Project.PYTORCH:
                    return (
                        <>
                            <strong>Command: 'Pyrefly check'</strong>
                            <br />
                            Pyrefly uses as many threads as possible, which was
                            "INSERT NUMBER HERE" on a 10 core machine.
                        </>
                    );
            }
        case TypeChecker.PYRE:
            return (
                <>
                    <strong>Command: 'pyre --no-saved-state check'</strong>
                    <br />
                    This uses X amount of threads on a 10 core machine
                </>
            );
        case TypeChecker.PYRIGHT:
            return (
                <>
                    <strong>Command: 'pyright --threads=8'</strong>
                    <br />
                    We picked 8 threads for pyright after trying a couple
                    options and found this to perform the fastest on a 10 core
                    machine.
                </>
            );
        case TypeChecker.MYPY:
            return (
                <>
                    <strong>Command: 'dmypy run on a 10 core machine'</strong>
                </>
            );
    }
}

const styles = stylex.create({
    tooltipContainer: {
        position: 'relative',
        display: 'inline-block',
        marginLeft: '8px',
    },
    infoIcon: {
        fontSize: '16px',
        color: 'var(--color-text)',
        cursor: 'help',
        verticalAlign: 'middle',
    },
    tooltip: {
        visibility: 'hidden',
        backgroundColor: 'var(--color-background-secondary)',
        color: 'var(--color-text)',
        textAlign: 'center',
        borderRadius: '6px',
        padding: '8px 12px',
        border: '1px solid var(--color-background)',
        position: 'absolute',
        zIndex: 1,
        bottom: '125%',
        left: '50%',
        transform: 'translateX(-50%)',
        width: '250px',
        opacity: 0,
        transition: 'opacity 0.3s',
    },
});

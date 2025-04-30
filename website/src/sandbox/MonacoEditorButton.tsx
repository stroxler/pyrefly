/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useState } from 'react';
import * as stylex from '@stylexjs/stylex';

export const BUTTON_HEIGHT = 40;

interface MonacoEditorButtonProps {
    id: string;
    onClick: () => Promise<void>;
    defaultLabel: string;
    runningLabel: string;
    ariaLabel?: string;
    isRunning?: boolean;
    setIsRunning?: React.Dispatch<React.SetStateAction<boolean>>;
}

export default function MonacoEditorButton({
    id,
    onClick,
    defaultLabel,
    runningLabel,
    ariaLabel,
    isRunning = undefined,
    setIsRunning = undefined,
}: MonacoEditorButtonProps): React.ReactElement {
    // if isRunning or setIsRunning are not provided, use local state
    if (isRunning === undefined || setIsRunning === undefined) {
        [isRunning, setIsRunning] = useState(false);
    }

    const disabled = isRunning;

    async function handleClick(): Promise<void> {
        if (disabled) return;

        try {
            setIsRunning(true);

            // Create a promise that resolves after 2 seconds
            const timerPromise = new Promise<void>((resolve) => {
                setTimeout(() => {
                    resolve();
                }, 2000);
            });

            // Wait for both the onClick function and the 2-second timer to complete
            Promise.all([onClick(), timerPromise]).then(() => {
                setIsRunning(false);
            });
        } catch (error) {
            console.error('Error in button action:', error);
        }
    }

    return (
        <button
            id={id}
            {...stylex.props(
                styles.buttonBase,
                isRunning ? styles.buttonRunning : styles.buttonDefault,
                disabled && styles.buttonDisabled
            )}
            onClick={handleClick}
            aria-label={ariaLabel || id}
            disabled={disabled}
        >
            <span {...stylex.props(styles.buttonText)}>
                {isRunning ? runningLabel : defaultLabel}
            </span>
        </button>
    );
}

// Define keyframes for animations
const runningKeyframes = stylex.keyframes({
    '0%': { transform: 'scale(1)' },
    '50%': { transform: 'scale(1.1)' },
    '100%': { transform: 'scale(1)' },
});

// Styles for MonacoEditorButton component
const styles = stylex.create({
    // Common button styles
    buttonBase: {
        display: 'flex',
        alignItems: 'center',
        borderRadius: '24px',
        cursor: 'pointer',
        transition: 'all 0.2s ease',
        zIndex: 1000,
        backdropFilter: 'blur(4px)',
        height: `${BUTTON_HEIGHT}px`,
        margin: '0 0 0 8px', // Add margin to separate buttons
        // Mobile styles (max-width: 768px)
        '@media (max-width: 768px)': {
            padding: '8px 16px',
            fontSize: '13px',
        },
        // Desktop styles (min-width: 769px)
        '@media (min-width: 769px)': {
            padding: '12px 20px',
            fontSize: '14px',
        },
    },
    // Default state
    buttonDefault: {
        background: 'rgba(255, 255, 255, 0.95)',
        border: '1px solid #ddd',
        color: 'inherit',
        boxShadow: '0 2px 8px rgba(0, 0, 0, 0.1)',
        '@media (min-width: 769px)': {
            ':hover': {
                background: 'rgba(255, 255, 255, 1)',
                transform: 'translateY(-1px)',
                boxShadow: '0 4px 12px rgba(0, 0, 0, 0.15)',
            },
        },
    },
    buttonText: {
        transition: 'opacity 0.2s ease, display 0s 0.2s',
    },
    // Running state
    buttonRunning: {
        background: 'rgba(76, 175, 80, 0.95)',
        color: 'white',
        border: '1px solid #43a047',
        boxShadow: '0 2px 8px rgba(0, 0, 0, 0.1)',
        animationName: runningKeyframes,
        animationDuration: '0.3s',
        animationTimingFunction: 'ease',
        '@media (min-width: 769px)': {
            ':hover': {
                background: 'rgba(67, 160, 71, 1)',
                color: 'white',
                transform: 'translateY(-1px)',
                boxShadow: '0 4px 12px rgba(0, 0, 0, 0.15)',
            },
        },
    },
    // Disabled state
    buttonDisabled: {
        cursor: 'not-allowed',
    },
});

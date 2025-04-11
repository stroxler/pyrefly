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
import Firefly from './firefly';
import typography from './typography';
import useBaseUrl from '@docusaurus/useBaseUrl';

export default function LandingPageHeader(): React.ReactElement {
    return (
        <header {...stylex.props(styles.featureHero)}>
            <section {...stylex.props(styles.logoContainer)}>
                <img
                    src={useBaseUrl('img/Pyrefly-Brandmark-Invert.svg')}
                    alt="Pyrefly Logo"
                    {...stylex.props(styles.logo)}
                />
            </section>
            <p {...stylex.props(styles.subtitle, typography.h3)}>
                <span>A faster Python type checker written in Rust</span>
            </p>
            <section {...stylex.props(styles.buttonGroup)}>
                <a
                    href="https://github.com/facebook/pyrefly/milestone/1"
                    {...stylex.props(styles.button, typography.p)}
                >
                    {' '}
                    Github{' '}
                </a>
                <a
                    href="https://pyrefly.org/try/"
                    {...stylex.props(styles.button, typography.p)}
                >
                    {' '}
                    Demo{' '}
                </a>
                <a
                    href="https://pyrefly.org/en/docs/learn-python-typing/"
                    {...stylex.props(styles.button, typography.p)}
                >
                    {' '}
                    Docs{' '}
                </a>
            </section>
            <section {...stylex.props(styles.buttonGroup)}>
                <p {...stylex.props(typography.p, typography.italic)}>
                    Launching Spring 2025
                </p>
            </section>
            <section>
                <Firefly />
                <Firefly />
                <Firefly />
                <Firefly />
                <Firefly />
            </section>
        </header>
    );
}

const styles = stylex.create({
    root: {
        '--title-font-size': '7rem',
        '--subtitle-font-size': '3.5rem',
    },
    logoContainer: {
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        marginBottom: '0.5rem',
    },
    logo: {
        height: '100px',
    },

    featureHero: {
        flex: 1,
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center', // Center content vertically
        width: '100%',
        height: '100%',
        padding: '7rem 0',
        position: 'relative',
        overflow: 'hidden',
        background: 'var(--color-background)',
        color: 'var(--color-text)',
        WebkitFontSmoothing: 'antialiased',
        marginLeft: 'auto',
        marginRight: 'auto',
        lineHeight: 1.1,
    },
    subtitle: {
        textAlign: 'center',
        marginBottom: '1rem',
    },
    link: {
        color: 'var(--color-primary)',
        textDecoration: 'underline',
        transition:
            'color var(--ifm-transition-fast) var(--ifm-transition-timing-default)',
        ':hover': {
            color: '#BA8E23',
            textDecoration: 'var(--ifm-link-decoration)',
        },
    },
    buttonGroup: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        marginTop: '20px',
    },
    button: {
        padding: '0.75rem 1.5rem',
        borderRadius: '4px',
        border: '1px solid var(--color-text)',
        backgroundColor: 'transparent',
        color: 'var(--color-text)',
        cursor: 'pointer',
        transition: 'all 0.2s',
        marginLeft: '10px',
        MarginRight: '10px',
    },
});

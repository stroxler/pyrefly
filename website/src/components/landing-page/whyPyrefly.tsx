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
import WhyPyreflyGridItem from './whyPyreflyGridItem';
import Firefly from './firefly';
import { useEffect, useState } from 'react';

export default function WhyPyrefly(): React.ReactElement {
    const [isLoaded, setIsLoaded] = useState(false);
    const [startAnimation, setStartAnimation] = useState(false);

    useEffect(() => {
        // Delay the animation to start after the header animations
        const timer = setTimeout(() => {
            setIsLoaded(true);

            // Start the grid items animation after the container animation
            const gridTimer = setTimeout(() => {
                setStartAnimation(true);
            }, 150); // Reduced from 300ms to 150ms

            return () => clearTimeout(gridTimer);
        }, 500); // Reduced from 800ms to 500ms

        return () => clearTimeout(timer);
    }, []);

    return (
        <div
            {...stylex.props(
                styles.whyPyreflyContainer,
                isLoaded && styles.whyPyreflyContainerVisible
            )}
        >
            <div {...stylex.props(styles.whyPyreflyGrid)}>
                {/* Short-term benefits */}
                <WhyPyreflyGridItem
                    title="Scale with Confidence"
                    content="Typecheck 1.8 million lines of code per second"
                    index={0}
                    startAnimation={startAnimation}
                />
                <WhyPyreflyGridItem
                    title="Developer Delight"
                    content="Catch errors as you write code, and get instant feedback right in your favorite editor"
                    index={1}
                    startAnimation={startAnimation}
                />
                <WhyPyreflyGridItem
                    title="Easy Onboarding"
                    content="Start type checking your code in minutes"
                    index={2}
                    startAnimation={startAnimation}
                />

                {/* Long-term benefits */}
                <WhyPyreflyGridItem
                    title="Codebase Clarity"
                    content="Improve code readability and maintainability with precise type annotations"
                    index={3}
                    startAnimation={startAnimation}
                />
                <WhyPyreflyGridItem
                    title="Tooling for Improving Types"
                    content="Comes packaged with tools to elevate your type coverage from 0 to 100"
                    index={4}
                    startAnimation={startAnimation}
                />
                <WhyPyreflyGridItem
                    title="Fully Featured"
                    content="Actively under development. Follows the official Python typing specification."
                    index={5}
                    startAnimation={startAnimation}
                />
            </div>
            <section {...stylex.props(styles.fireflyContainer)}>
                <Firefly />
                <Firefly />
                <Firefly />
            </section>
        </div>
    );
}

const styles = stylex.create({
    whyPyreflyContainer: {
        position: 'relative',
        paddingTop: '1.5rem',
        paddingBottom: '2rem',
        opacity: 0,
        filter: 'blur(8px)',
        transform: 'translateY(20px)',
        transition: 'all 0.8s cubic-bezier(0.34, 1.56, 0.64, 1)', // Reduced from 1s to 0.8s
    },
    whyPyreflyContainerVisible: {
        opacity: 1,
        filter: 'blur(0px)',
        transform: 'translateY(0)',
    },
    whyPyreflyGrid: {
        display: 'grid',
        gridTemplateColumns: 'repeat(3, 1fr)',
        gap: '1.5rem',
        position: 'relative',
        zIndex: 2,
        '@media (max-width: 800px)': {
            gridTemplateColumns: 'repeat(2, 1fr)',
        },
        '@media (max-width: 550px)': {
            gridTemplateColumns: 'repeat(1, 1fr)',
        },
    },
    fireflyContainer: {
        position: 'absolute',
        top: 0,
        left: 0,
        width: '100%',
        height: '100%',
        zIndex: 1,
        overflow: 'hidden',
    },
});

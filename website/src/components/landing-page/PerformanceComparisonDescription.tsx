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
import typography from './typography';

export default function PerformanceComparisonDescription(): React.ReactElement {
    return (
        <div {...stylex.props(styles.messageContainer, typography.h5)}>
            Typechecking the instagram codebase with 19 millions lines of code,
            from scratch.
        </div>
    );
}

const styles = stylex.create({
    messageContainer: {
        paddingBottom: '2rem',
    },
});

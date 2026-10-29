# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import sys

DEFAULTS = {"debug": False}
DEFAULTS["debug"] = True
CONFIG = json.loads("{}")


def reload_plugins():
    sys.modules.pop("plugin", None)

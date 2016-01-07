// module Pulp.System.SupportsColor
"use strict";

// The MIT License (MIT)
// 
// Copyright (c) Sindre Sorhus <sindresorhus@gmail.com> (sindresorhus.com)
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

exports.supportLevel = (function() {
  if (process.stderr && !process.stderr.isTTY) {
    return 0;
  }

  if (process.platform === 'win32') {
    return 1;
  }

  if ('COLORTERM' in process.env) {
    return 1;
  }

  if (process.env.TERM === 'dumb') {
    return 0;
  }

  if (/^xterm-256(?:color)?/.test(process.env.TERM)) {
    return 2;
  }

  if (/^screen|^xterm|^vt100|color|ansi|cygwin|linux/i.test(process.env.TERM)) {
    return 1;
  }

  return 0;
})();

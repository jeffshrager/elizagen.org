# CTSS SLIP

This is an implementation of MAD-SLIP on CTSS found among Professor
Joseph Weizenbaum's papers at MIT. The original source can be found
[here](https://dome.mit.edu/handle/1721.3/201707)

The [PDF](02-000311065.pdf) file contains a listing of FAP and MAD
 code. 
 
A [transcription](02-000311065.mad) of this to a text file was done by
 Arthur Schwarz. We have started to break this down into individual
 files under the following directories.
 
- [`SLIP-core`](SLIP-core) - baseline SLIP code matching what was mentioned in
the CACM article.
- [`SLIP-eliza`](SLIP-eliza) - extra functions we believe were added for ELIZA
- [`SLIP-fap`](SLIP-fap) - FAP assembly language primitives
- [`SLIP-duplicates`](SLIP-duplicates) - where there were items
  duplicated in both MAD and FAP, we have used the MAD version and
  placed the FAP duplicate in here
- [`SLIP-discarded`](SLIP-discarded) - incomplete functions not called
  by the rest of the code.
 
## License

 From
 [dome.mit.edu](https://dome.mit.edu/bitstream/handle/1721.3/201707/LICENSE.txt?sequence=2&isAllowed=y)

Copyright 1965 MIT

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



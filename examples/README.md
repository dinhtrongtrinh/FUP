# Solved Examples

Contains bitmaps and their solutions, along with an example wrapper application
for converting the bitmap files into ASCII art. To try your solution, You must
place the example app and your library into the same folder.

You can check the correctness of your solution with
```sh
# expects the arguments: box-width, box-height and a bitmap path.
racket ./example-app.rkt 8 16 ./eye.png | diff -w ./eye.ascii.txt -
```
The command will return no output if it finds no differences.

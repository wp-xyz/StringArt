# StringArt

Suppose you have a disk, about 50 cm in diameter, and you put in a large number of equally-spaced nails at the perimeter. How should a long wire or thread be wound between any of these nails so that a given image is best approximated?

This task is solved to some degree by this project:
* Define the number of nails
* Load an "arbitrary" image.
* The application begins at one specific nail, draws an imaginary line to the next nail and calculates the average gray value along this line. This is repeated with the line to the adjacent nail etc., until all nail have been tested. The line which has the lowest gray value defines the nail to which the wire will have to be drawn. The work image is erased along this line so that it will not be used again. Then the game continues in the same way until the final image is appearing.
* The program outputs the simulated image as well as a list of the nail positions which have to be connected in the specified order.

The word "arbitrary" mentioned above is a bit too ambitious. Images with too many details, too little contrast, or contrast at the wrong place will not be rendered well by this technique. And even with the "good" ones it is important to find the correct number of connections: if there are too few connetions significant details are not yet resolved, if there are too many the image becomes too dark.

![screenshot_stringart](https://github.com/wp-xyz/StringArt/assets/30792460/bf2dbacd-56a6-425e-a566-d7e362b87afe)

# Compilation
The project is written in Object Pascal and must be compiled with a recent Lazarus/FreePascal version (tests back to Lazarus v2.0 and FPC 3.0 were successful). Additional libraries and packages are not required. The code should compile for Windows (tested: Windows 11), Linux and Mac.

# xl2fpcore
Converts Excel formulas to FPCore expressions

## HOWTO

1. Clone this repository recursively; it uses git submodules.  E.g,  
	```
	$ git clone --recursive git@github.com:dbarowy/xl2fpcore.git
	```
2. Open the `xl2fpcore.sln` solution using Visual Studio.
3. Make sure that the `xl2fpcore console` is set as your startup project.
4. Click the `Start` button in Visual Studio to compile and run.

## Troubleshooting

+ If you see errors about missing files and folders under `parcel/`,
  you did not clone recursively. Either reclone or execute
  ```
  git clone https://github.com/plasma-umass/parcel.git parcel/
  ```
  
+ If the build process complains about missing the `FPCore` namespace,
  you likely have a version conflict. In the Solution Explorer, locate
  the Project that has a problem, right-click on References, Add a
  NuGet Package, and add `FSharp.Core`.

## Using as a library

To use this project as a library, import the `xl2fpcore`, `ExcelParser`, `FParsec`, and `FParsecCS` projects.

Note that both [FParsec](http://www.quanttec.com/fparsec/license.html)
and [Parcel](https://github.com/plasma-umass/parcel/blob/1a63cf1695b6e50c56a5c81cabb34717f7635015/LICENSE.txt)
have their own licenses.

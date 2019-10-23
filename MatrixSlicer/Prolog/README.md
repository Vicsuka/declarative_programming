<h1> Matrix slicer in Prolog </h1>

<h2>Usage:</h2>
  <h3>Call with parameters:</h3> <br>
    khf1:feldarabolasa(<strong>Matrix, RowCount-ColumnCount,Output</strong>).<br>
    <br>
    Where <strong>Matrix</strong> is the matrix you want to slice up to smaller matrixes in ListofLists format
    and <strong>RowCount</strong> and <strong>ColumnCount</strong> is the desired size of the smaller matrixes.<br>
    Output will be our output list of submatrixes.
  <h3>Example:</h3><br>
    | ?- feldarabolasa([[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]], 2-2, Output).<br>
  <h3>Output:</h3><br>
    The list of submatrixes.(e.g. Output = [[a,b,e,f],[c,d,g,h],[i,j,m,n],[k,l,o,p]])<br>

<h1> Matrix slicer in Erlang </h1>

<h2>Usage:</h2>
  <h3>Compile:</h3><br>
    > c(khf1).<br>
  <h3>Call with parameters:</h3> <br>
    khf1:feldarabolasa(<strong>Matrix, {RowCount,ColumnCount}</strong>).<br>
    <br>
    Where <strong>Matrix</strong> is the matrix you want to slice up to smaller matrixes in ListofLists format
    and <strong>RowCount</strong> and <strong>ColumnCount</strong> is the desired size of the smaller matrixes.<br>
  <h3>Example:</h3><br>
    > khf1:feldarabolasa([[a,b,c,d],[e,f,g,h]],{2,2}).<br>
  <h3>Output:</h3><br>
    The list of submatrixes.(e.g. [[a,b,e,f],[c,d,g,h]]) <br>

let record_update=fun cell->{(!cell)with count=(!cell).count+1}
let loop=fun stop->let found=ref false in let index=ref 0 in while Int.((!index)<stop)&&not(!found) do index:=Int.add(!index)1 done;!found

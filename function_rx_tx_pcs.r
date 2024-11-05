

function_rx_tx_return_pcrx_pctx=function(rx,tx){

xxt=tcrossprod(rx)
 ea=eigen(xxt)
  D=ea$values
  U=ea$vectors
  
#delete the last
  U_passed=U[,-nrow(U)]
  D_passed=D[-nrow(U)]

#delta
  delta=sqrt(D_passed)
  s_delta=1/delta
  
#vmatrix  
  vmatrix_middle=crossprod(rx,U_passed)
  vmatrix=vmatrix_middle%*%diag(s_delta)
  
#pcs  
pc_rx=U_passed%*%diag(delta)
pc_tx=tx%*%vmatrix

return(list(pc_rx,pc_tx))
}



function_x_return_pc=function(x_ori){

xxt=tcrossprod(x_ori)
 ea=eigen(xxt)
  D=ea$values
  U=ea$vectors
  
#delete the last
  U_passed=U[,-nrow(U)]
  D_passed=D[-nrow(U)]

#delta
  delta=sqrt(D_passed)
  
#pcs  
pc=U_passed%*%diag(delta)

return(pc)
}



# xxt=svd(rx)
# vmatrix=xxt$v
# pc_rx=rx%*%vmatrix
# pc_tx=tx%*%vmatrix

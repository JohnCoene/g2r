function get_eval(arr){
  if(typeof arr != "undefined")
    if(typeof arr == "function")
      return eval(arr[0])
    else 
      return arr
  return arr
}
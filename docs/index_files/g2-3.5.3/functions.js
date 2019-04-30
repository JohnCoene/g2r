function get_eval(arr){
  if(typeof arr != "undefined")
    if(typeof arr[0] == "function")
      return eval(arr[0])
    else 
      return arr
  return arr
}

function get_json(url, success) {

  var ud = '_' + +new Date,
      script = document.createElement('script'),
      head = document.getElementsByTagName('head')[0] 
             || document.documentElement;

  window[ud] = function(data) {
      head.removeChild(script);
      success && success(data);
  };

  script.src = url.replace('callback=?', 'callback=' + ud);
  head.appendChild(script);

}

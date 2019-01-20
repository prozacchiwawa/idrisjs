(function(){

function putInQueue(value, queue){
  if(queue.callback == undefined){
    queue.queue.push(value);
  }else{
    var callB = queue.callback;
    queue.callback = undefined;
    setTimeout(function() { callB(value); }, 0);
  }
}

function getFromQueue(queue, callback){
  if(queue.queue.length > 0){
    callback(queue.queue.shift());
  }else{
    queue.callback = callback;
  }
}

this.$JSLIB$async = {
  putInQueue: putInQueue,
  getFromQueue : getFromQueue
};

}).call(this);

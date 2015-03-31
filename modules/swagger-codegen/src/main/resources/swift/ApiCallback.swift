class APICallback<T> {
    
  var success : (T!) -> Void
  var fail : (APIException) -> Void
  
  init(success : (T!) -> Void, fail : (APIException) -> Void) {
    self.success = success
    self.fail = fail
  }
  
  func onSuccess(result: T!) {
    success(result)
  }
  
  func onFailure(exception: APIException) {
    fail(exception)
  }
}
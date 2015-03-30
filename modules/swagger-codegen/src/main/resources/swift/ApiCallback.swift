class ApiCallback<T> {
    
  var success : (T!) -> Void
  var fail : (ApiException) -> Void
  
  init(success : (T!) -> Void, fail : (ApiException) -> Void) {
    self.success = success
    self.fail = fail
  }
  
  func onSuccess(result: T!) {
    success(result)
  }
  
  func onFailure(exception: ApiException) {
    fail(exception)
  }
}
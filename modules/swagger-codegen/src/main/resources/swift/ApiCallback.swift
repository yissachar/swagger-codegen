class ApiCallback<T> {
    
  var success : (T!) -> Void
  var fail : () -> Void
  
  init(success : (T!) -> Void, fail : () -> Void) {
    self.success = success
    self.fail = fail
  }
  
  func onSuccess(result: T!) {
    success(result)
  }
  
  func onFailure() {
    fail()
  }
}
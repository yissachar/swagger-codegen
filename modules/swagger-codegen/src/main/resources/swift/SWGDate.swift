class SWGDate {

  var date:NSDate

  init(input: String) {        
    var df: NSDateFormatter = NSDateFormatter()
    df.timeZone = NSTimeZone.defaultTimeZone()
    df.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
    date = df.dateFromString(input)!
  }

  func toString() -> String {
    var df: NSDateFormatter = NSDateFormatter()
    df.timeZone = NSTimeZone.defaultTimeZone();
    df.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
    return df.stringFromDate(date)
  }
  
}
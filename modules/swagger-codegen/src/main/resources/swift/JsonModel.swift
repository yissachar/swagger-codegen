class JSONModel {  
  
  func toJson() -> String {        
    fatalError("This method must be overridden")
  }

  func getProperties() -> [String: MirrorType] {
    let reflected = reflect(self)
    
    var properties: [String: MirrorType] = [String: MirrorType]()
    var numProperties = reflected.count
    for index in 0..<numProperties {
      let (property, mirror) = reflected[index]
      if property != "super" {
        properties[property] = mirror
      }
    }
    
    return properties
  }

  func jsonify(object: AnyObject?) -> String {
    var json: String = ""

    if let str = object as? String {
      json += "\"\(str)\""
    } else if let model = object as? JSONModel {
      json += model.toJson()
    } else if let array = object as? [AnyObject] {
      json += "["
        
      var prefix: String = ""
      for item in array {
        json += prefix
        json += jsonify(item)
        prefix = ","
      }
      
      json += "]"
    } else if object != nil {
      json += "\(object!)"
    }

    return json
  }
}
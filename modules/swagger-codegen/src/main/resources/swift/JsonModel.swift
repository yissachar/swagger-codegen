class JsonModel: NSObject {

  init(json: NSDictionary = NSDictionary()) {
    super.init()

    let properties: [String: MirrorType] = getProperties()
    for (jsonKey, jsonValue) in json {
      let key: String = jsonKey as String
      
      if (respondsToSelector(NSSelectorFromString(key))) {
        var value: AnyObject? = jsonValue
        let mirrorType = properties[key]

        if value is NSDictionary || value is NSArray {
          value = ApiInvoker.deserialize(jsonValue, type: mirrorType!.valueType)
        }

        setValue(value, forKey: key)
      }
    }
  }
  
  func toJson() -> String {        
    return jsonify(self)
  }

  func getProperties() -> [String: MirrorType] {
    let reflected = reflect(self)
    
    var properties: [String: MirrorType] = [String: MirrorType]()
    var numProperties = reflected.count
    for index in 0..<numProperties {
      let (property, mirror) = reflected[index]
      if property != "super" && property != "id" {
        properties[property] = mirror
      }
    }
    
    return properties
  }

  func jsonify(object: AnyObject) -> String {
    var json: String = ""

    if object is String {
      json += "\"\(object)\""
    } else if object is JsonModel {
      json += "{"
        
      var prefix: String = ""
      for property in (object as JsonModel).getProperties().keys {
        if let value: AnyObject = (object as JsonModel).valueForKey(property) {
          json += prefix
          json += "\"\(property)\": "
          json += jsonify(value)
          prefix = ","
        }
      }
      
      json += "}"
    } else if object is [AnyObject] {
      json += "["
        
      var prefix: String = ""
      for item in (object as [AnyObject]) {
        json += prefix
        json += jsonify(item)
        prefix = ","
      }
      
      json += "]"
    } else {
      json += "\(object)"
    }

    return json
  }
}
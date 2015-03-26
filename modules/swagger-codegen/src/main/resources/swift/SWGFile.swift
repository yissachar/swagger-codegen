class SWGFile {
	var name: String
	var mimeType: String
	var data: String

	init(filename: String, fileMimeType: String, data: String) {
		name = filename
		mimeType = fileMimeType
		self.data = data
	}
}
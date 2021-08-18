//
//  CodableHelperTests.swift
//  SwaggerClientTests
//
//  Created by Plamen Andreev on 9/24/19.
//  Copyright © 2019 Swagger. All rights reserved.
//

import PetstoreClient
import XCTest

class CodableHelperTests: XCTestCase {
    func testDecodeMultipeHandlesDateFormats() {
        let jsonStrings = [
            "{\"date\": \"2019-02-14\"}",
            "{\"date\": \"2019-02-14 00:00:00\"}",
            "{\"date\": \"2019-02-14T00:00:00\"}",
            "{\"date\": \"2019-02-14T00:00:00+0100\"}",
            "{\"date\": \"2019-02-14T00:00:00.000+0100\"}",
            "{\"date\": \"2019-02-14T00:00:00Z\"}",
            "{\"date\": \"2019-02-14T00:00:00.000\"}",
            "{\"date\": \"2019-02-14T00:00:00.000Z\"}",
        ]

        for jsonString in jsonStrings {
            // Given
            let jsonData = jsonString.data(using: .utf8)!

            // When
            let model = CodableHelper.decode(ModelWithDate.self, from: jsonData)

            // Then
            XCTAssertNotNil(model.decodableObj)
            XCTAssertNil(model.error)
        }
    }
}

struct ModelWithDate: Codable {
    var date: Date
}

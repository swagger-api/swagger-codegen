/*
 * Currency.h
 *
 * some description 
 */

#ifndef _Currency_H_
#define _Currency_H_


#include <string>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief some description 
 *
 *  \ingroup Models
 *
 */

class Currency : public Object {
public:
	/*! \brief Constructor.
	 */
	Currency();
	Currency(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Currency();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);


private:
	void __init();
	void __cleanup();

};
}
}

#endif /* _Currency_H_ */

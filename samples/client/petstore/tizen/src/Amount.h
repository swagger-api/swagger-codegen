/*
 * Amount.h
 *
 * some description 
 */

#ifndef _Amount_H_
#define _Amount_H_


#include <string>
#include "Currency.h"
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

class Amount : public Object {
public:
	/*! \brief Constructor.
	 */
	Amount();
	Amount(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Amount();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);

	/*! \brief Get some description 
	 */
	double getValue();

	/*! \brief Set some description 
	 */
	void setValue(double  value);
	/*! \brief Get 
	 */
	Currency getCurrency();

	/*! \brief Set 
	 */
	void setCurrency(Currency  currency);

private:
	double value;
	Currency currency;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Amount_H_ */

/**
 * Swagger Petstore
 * This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 * OpenAPI spec version: 1.0.0
 * Contact: apiteam@swagger.io
 *
 * NOTE: This class is auto generated by the swagger code generator 2.4.3-SNAPSHOT.
 * https://github.com/swagger-api/swagger-codegen.git
 * Do not edit the class manually.
 */



#include "Order.h"

#include <string>
#include <sstream>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

using boost::property_tree::ptree;
using boost::property_tree::read_json;
using boost::property_tree::write_json;

namespace io {
namespace swagger {
namespace server {
namespace model {

Order::Order()
{
    m_Id = 0L;
    m_PetId = 0L;
    m_Quantity = 0;
    m_ShipDate = "";
    m_Status = "";
    m_Complete = false;
    
}

Order::~Order()
{
}

std::string Order::toJsonString()
{
	std::stringstream ss;
	ptree pt;
	pt.put("Id", m_Id);
	pt.put("PetId", m_PetId);
	pt.put("Quantity", m_Quantity);
	pt.put("ShipDate", m_ShipDate);
	pt.put("Status", m_Status);
	pt.put("Complete", m_Complete);
	write_json(ss, pt, false);
	return ss.str();
}

void Order::fromJsonString(std::string const& jsonString)
{
	std::stringstream ss(jsonString);
	ptree pt;
	read_json(ss,pt);
	m_Id = pt.get("Id", 0L);
	m_PetId = pt.get("PetId", 0L);
	m_Quantity = pt.get("Quantity", 0);
	m_ShipDate = pt.get("ShipDate", "");
	m_Status = pt.get("Status", "");
	m_Complete = pt.get("Complete", false);
}

int64_t Order::getId() const
{
    return m_Id;
}
void Order::setId(int64_t value)
{
    m_Id = value;
}
int64_t Order::getPetId() const
{
    return m_PetId;
}
void Order::setPetId(int64_t value)
{
    m_PetId = value;
}
int32_t Order::getQuantity() const
{
    return m_Quantity;
}
void Order::setQuantity(int32_t value)
{
    m_Quantity = value;
}
std::string Order::getShipDate() const
{
    return m_ShipDate;
}
void Order::setShipDate(std::string value)
{
    m_ShipDate = value;
}
std::string Order::getStatus() const
{
    return m_Status;
}
void Order::setStatus(std::string value)
{
    m_Status = value;
}
bool Order::isComplete() const
{
    return m_Complete;
}
void Order::setComplete(bool value)
{
    m_Complete = value;
}

}
}
}
}



package io.swagger.client.model


case class Amount (
    /* some description  */
    _value: Double,
    _currency: Currency
)
object Amount {
    def toStringBody(var_value: Object, var_currency: Object) =
        s"""
        | {
        | "value":$var_value,"currency":$var_currency
        | }
        """.stripMargin
}

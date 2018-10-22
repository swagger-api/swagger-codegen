using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// some description 
    /// </summary>
    public sealed class Amount:  IEquatable<Amount>
    { 
        /// <summary>
        /// some description 
        /// </summary>
        public double? Value { get; private set; }

        /// <summary>
        /// Currency
        /// </summary>
        public Currency Currency { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Amount.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Amount()
        {
        }

        private Amount(double? Value, Currency Currency)
        {
            
            this.Value = Value;
            
            this.Currency = Currency;
            
        }

        /// <summary>
        /// Returns builder of Amount.
        /// </summary>
        /// <returns>AmountBuilder</returns>
        public static AmountBuilder Builder()
        {
            return new AmountBuilder();
        }

        /// <summary>
        /// Returns AmountBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>AmountBuilder</returns>
        public AmountBuilder With()
        {
            return Builder()
                .Value(Value)
                .Currency(Currency);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Amount other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Amount.
        /// </summary>
        /// <param name="left">Compared (Amount</param>
        /// <param name="right">Compared (Amount</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Amount left, Amount right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Amount.
        /// </summary>
        /// <param name="left">Compared (Amount</param>
        /// <param name="right">Compared (Amount</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Amount left, Amount right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Amount.
        /// </summary>
        public sealed class AmountBuilder
        {
            private double? _Value;
            private Currency _Currency;

            internal AmountBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Amount.Value property.
            /// </summary>
            /// <param name="value">some description </param>
            public AmountBuilder Value(double? value)
            {
                _Value = value;
                return this;
            }

            /// <summary>
            /// Sets value for Amount.Currency property.
            /// </summary>
            /// <param name="value">Currency</param>
            public AmountBuilder Currency(Currency value)
            {
                _Currency = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Amount.
            /// </summary>
            /// <returns>Amount</returns>
            public Amount Build()
            {
                Validate();
                return new Amount(
                    Value: _Value,
                    Currency: _Currency
                );
            }

            private void Validate()
            { 
                if (_Value == null)
                {
                    throw new ArgumentException("Value is a required property for Amount and cannot be null");
                } 
                if (_Currency == null)
                {
                    throw new ArgumentException("Currency is a required property for Amount and cannot be null");
                } 
            }
        }

        
    }
}
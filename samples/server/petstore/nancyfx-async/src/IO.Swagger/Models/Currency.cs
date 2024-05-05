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
    public sealed class Currency:  IEquatable<Currency>
    { 

        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Currency.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Currency()
        {
        }

        private Currency()
        {
            
        }

        /// <summary>
        /// Returns builder of Currency.
        /// </summary>
        /// <returns>CurrencyBuilder</returns>
        public static CurrencyBuilder Builder()
        {
            return new CurrencyBuilder();
        }

        /// <summary>
        /// Returns CurrencyBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>CurrencyBuilder</returns>
        public CurrencyBuilder With()
        {
            return Builder()
;
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Currency other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Currency.
        /// </summary>
        /// <param name="left">Compared (Currency</param>
        /// <param name="right">Compared (Currency</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Currency left, Currency right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Currency.
        /// </summary>
        /// <param name="left">Compared (Currency</param>
        /// <param name="right">Compared (Currency</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Currency left, Currency right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Currency.
        /// </summary>
        public sealed class CurrencyBuilder
        {

            internal CurrencyBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }


            /// <summary>
            /// Builds instance of Currency.
            /// </summary>
            /// <returns>Currency</returns>
            public Currency Build()
            {
                Validate();
                return new Currency(
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
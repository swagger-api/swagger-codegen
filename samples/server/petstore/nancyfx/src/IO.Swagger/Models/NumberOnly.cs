using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// NumberOnly
    /// </summary>
    public sealed class NumberOnly:  IEquatable<NumberOnly>
    { 
        /// <summary>
        /// JustNumber
        /// </summary>
        public decimal? JustNumber { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use NumberOnly.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public NumberOnly()
        {
        }

        private NumberOnly(decimal? JustNumber)
        {
            
            this.JustNumber = JustNumber;
            
        }

        /// <summary>
        /// Returns builder of NumberOnly.
        /// </summary>
        /// <returns>NumberOnlyBuilder</returns>
        public static NumberOnlyBuilder Builder()
        {
            return new NumberOnlyBuilder();
        }

        /// <summary>
        /// Returns NumberOnlyBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>NumberOnlyBuilder</returns>
        public NumberOnlyBuilder With()
        {
            return Builder()
                .JustNumber(JustNumber);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(NumberOnly other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (NumberOnly.
        /// </summary>
        /// <param name="left">Compared (NumberOnly</param>
        /// <param name="right">Compared (NumberOnly</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (NumberOnly left, NumberOnly right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (NumberOnly.
        /// </summary>
        /// <param name="left">Compared (NumberOnly</param>
        /// <param name="right">Compared (NumberOnly</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (NumberOnly left, NumberOnly right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of NumberOnly.
        /// </summary>
        public sealed class NumberOnlyBuilder
        {
            private decimal? _JustNumber;

            internal NumberOnlyBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for NumberOnly.JustNumber property.
            /// </summary>
            /// <param name="value">JustNumber</param>
            public NumberOnlyBuilder JustNumber(decimal? value)
            {
                _JustNumber = value;
                return this;
            }


            /// <summary>
            /// Builds instance of NumberOnly.
            /// </summary>
            /// <returns>NumberOnly</returns>
            public NumberOnly Build()
            {
                Validate();
                return new NumberOnly(
                    JustNumber: _JustNumber
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// ArrayOfNumberOnly
    /// </summary>
    public sealed class ArrayOfNumberOnly:  IEquatable<ArrayOfNumberOnly>
    { 
        /// <summary>
        /// ArrayNumber
        /// </summary>
        public List<decimal?> ArrayNumber { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ArrayOfNumberOnly.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ArrayOfNumberOnly()
        {
        }

        private ArrayOfNumberOnly(List<decimal?> ArrayNumber)
        {
            
            this.ArrayNumber = ArrayNumber;
            
        }

        /// <summary>
        /// Returns builder of ArrayOfNumberOnly.
        /// </summary>
        /// <returns>ArrayOfNumberOnlyBuilder</returns>
        public static ArrayOfNumberOnlyBuilder Builder()
        {
            return new ArrayOfNumberOnlyBuilder();
        }

        /// <summary>
        /// Returns ArrayOfNumberOnlyBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ArrayOfNumberOnlyBuilder</returns>
        public ArrayOfNumberOnlyBuilder With()
        {
            return Builder()
                .ArrayNumber(ArrayNumber);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ArrayOfNumberOnly other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ArrayOfNumberOnly.
        /// </summary>
        /// <param name="left">Compared (ArrayOfNumberOnly</param>
        /// <param name="right">Compared (ArrayOfNumberOnly</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ArrayOfNumberOnly left, ArrayOfNumberOnly right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ArrayOfNumberOnly.
        /// </summary>
        /// <param name="left">Compared (ArrayOfNumberOnly</param>
        /// <param name="right">Compared (ArrayOfNumberOnly</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ArrayOfNumberOnly left, ArrayOfNumberOnly right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ArrayOfNumberOnly.
        /// </summary>
        public sealed class ArrayOfNumberOnlyBuilder
        {
            private List<decimal?> _ArrayNumber;

            internal ArrayOfNumberOnlyBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ArrayOfNumberOnly.ArrayNumber property.
            /// </summary>
            /// <param name="value">ArrayNumber</param>
            public ArrayOfNumberOnlyBuilder ArrayNumber(List<decimal?> value)
            {
                _ArrayNumber = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ArrayOfNumberOnly.
            /// </summary>
            /// <returns>ArrayOfNumberOnly</returns>
            public ArrayOfNumberOnly Build()
            {
                Validate();
                return new ArrayOfNumberOnly(
                    ArrayNumber: _ArrayNumber
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

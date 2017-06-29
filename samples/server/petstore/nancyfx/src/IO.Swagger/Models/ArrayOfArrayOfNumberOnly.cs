using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// ArrayOfArrayOfNumberOnly
    /// </summary>
    public sealed class ArrayOfArrayOfNumberOnly:  IEquatable<ArrayOfArrayOfNumberOnly>
    { 
        /// <summary>
        /// ArrayArrayNumber
        /// </summary>
        public List<List<decimal?>> ArrayArrayNumber { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ArrayOfArrayOfNumberOnly.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ArrayOfArrayOfNumberOnly()
        {
        }

        private ArrayOfArrayOfNumberOnly(List<List<decimal?>> ArrayArrayNumber)
        {
            
            this.ArrayArrayNumber = ArrayArrayNumber;
            
        }

        /// <summary>
        /// Returns builder of ArrayOfArrayOfNumberOnly.
        /// </summary>
        /// <returns>ArrayOfArrayOfNumberOnlyBuilder</returns>
        public static ArrayOfArrayOfNumberOnlyBuilder Builder()
        {
            return new ArrayOfArrayOfNumberOnlyBuilder();
        }

        /// <summary>
        /// Returns ArrayOfArrayOfNumberOnlyBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ArrayOfArrayOfNumberOnlyBuilder</returns>
        public ArrayOfArrayOfNumberOnlyBuilder With()
        {
            return Builder()
                .ArrayArrayNumber(ArrayArrayNumber);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ArrayOfArrayOfNumberOnly other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ArrayOfArrayOfNumberOnly.
        /// </summary>
        /// <param name="left">Compared (ArrayOfArrayOfNumberOnly</param>
        /// <param name="right">Compared (ArrayOfArrayOfNumberOnly</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ArrayOfArrayOfNumberOnly left, ArrayOfArrayOfNumberOnly right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ArrayOfArrayOfNumberOnly.
        /// </summary>
        /// <param name="left">Compared (ArrayOfArrayOfNumberOnly</param>
        /// <param name="right">Compared (ArrayOfArrayOfNumberOnly</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ArrayOfArrayOfNumberOnly left, ArrayOfArrayOfNumberOnly right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ArrayOfArrayOfNumberOnly.
        /// </summary>
        public sealed class ArrayOfArrayOfNumberOnlyBuilder
        {
            private List<List<decimal?>> _ArrayArrayNumber;

            internal ArrayOfArrayOfNumberOnlyBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ArrayOfArrayOfNumberOnly.ArrayArrayNumber property.
            /// </summary>
            /// <param name="value">ArrayArrayNumber</param>
            public ArrayOfArrayOfNumberOnlyBuilder ArrayArrayNumber(List<List<decimal?>> value)
            {
                _ArrayArrayNumber = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ArrayOfArrayOfNumberOnly.
            /// </summary>
            /// <returns>ArrayOfArrayOfNumberOnly</returns>
            public ArrayOfArrayOfNumberOnly Build()
            {
                Validate();
                return new ArrayOfArrayOfNumberOnly(
                    ArrayArrayNumber: _ArrayArrayNumber
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

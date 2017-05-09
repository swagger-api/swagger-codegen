using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// ArrayTest
    /// </summary>
    public sealed class ArrayTest:  IEquatable<ArrayTest>
    { 
        /// <summary>
        /// ArrayOfString
        /// </summary>
        public List<string> ArrayOfString { get; private set; }

        /// <summary>
        /// ArrayArrayOfInteger
        /// </summary>
        public List<List<long?>> ArrayArrayOfInteger { get; private set; }

        /// <summary>
        /// ArrayArrayOfModel
        /// </summary>
        public List<List<ReadOnlyFirst>> ArrayArrayOfModel { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ArrayTest.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ArrayTest()
        {
        }

        private ArrayTest(List<string> ArrayOfString, List<List<long?>> ArrayArrayOfInteger, List<List<ReadOnlyFirst>> ArrayArrayOfModel)
        {
            
            this.ArrayOfString = ArrayOfString;
            
            this.ArrayArrayOfInteger = ArrayArrayOfInteger;
            
            this.ArrayArrayOfModel = ArrayArrayOfModel;
            
        }

        /// <summary>
        /// Returns builder of ArrayTest.
        /// </summary>
        /// <returns>ArrayTestBuilder</returns>
        public static ArrayTestBuilder Builder()
        {
            return new ArrayTestBuilder();
        }

        /// <summary>
        /// Returns ArrayTestBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ArrayTestBuilder</returns>
        public ArrayTestBuilder With()
        {
            return Builder()
                .ArrayOfString(ArrayOfString)
                .ArrayArrayOfInteger(ArrayArrayOfInteger)
                .ArrayArrayOfModel(ArrayArrayOfModel);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ArrayTest other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ArrayTest.
        /// </summary>
        /// <param name="left">Compared (ArrayTest</param>
        /// <param name="right">Compared (ArrayTest</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ArrayTest left, ArrayTest right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ArrayTest.
        /// </summary>
        /// <param name="left">Compared (ArrayTest</param>
        /// <param name="right">Compared (ArrayTest</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ArrayTest left, ArrayTest right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ArrayTest.
        /// </summary>
        public sealed class ArrayTestBuilder
        {
            private List<string> _ArrayOfString;
            private List<List<long?>> _ArrayArrayOfInteger;
            private List<List<ReadOnlyFirst>> _ArrayArrayOfModel;

            internal ArrayTestBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ArrayTest.ArrayOfString property.
            /// </summary>
            /// <param name="value">ArrayOfString</param>
            public ArrayTestBuilder ArrayOfString(List<string> value)
            {
                _ArrayOfString = value;
                return this;
            }

            /// <summary>
            /// Sets value for ArrayTest.ArrayArrayOfInteger property.
            /// </summary>
            /// <param name="value">ArrayArrayOfInteger</param>
            public ArrayTestBuilder ArrayArrayOfInteger(List<List<long?>> value)
            {
                _ArrayArrayOfInteger = value;
                return this;
            }

            /// <summary>
            /// Sets value for ArrayTest.ArrayArrayOfModel property.
            /// </summary>
            /// <param name="value">ArrayArrayOfModel</param>
            public ArrayTestBuilder ArrayArrayOfModel(List<List<ReadOnlyFirst>> value)
            {
                _ArrayArrayOfModel = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ArrayTest.
            /// </summary>
            /// <returns>ArrayTest</returns>
            public ArrayTest Build()
            {
                Validate();
                return new ArrayTest(
                    ArrayOfString: _ArrayOfString,
                    ArrayArrayOfInteger: _ArrayArrayOfInteger,
                    ArrayArrayOfModel: _ArrayArrayOfModel
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

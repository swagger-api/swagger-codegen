using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// EnumTest
    /// </summary>
    public sealed class EnumTest:  IEquatable<EnumTest>
    { 
        /// <summary>
        /// EnumString
        /// </summary>
        public EnumStringEnum? EnumString { get; private set; }

        /// <summary>
        /// EnumInteger
        /// </summary>
        public EnumIntegerEnum? EnumInteger { get; private set; }

        /// <summary>
        /// EnumNumber
        /// </summary>
        public EnumNumberEnum? EnumNumber { get; private set; }

        /// <summary>
        /// OuterEnum
        /// </summary>
        public OuterEnum OuterEnum { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use EnumTest.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public EnumTest()
        {
        }

        private EnumTest(EnumStringEnum? EnumString, EnumIntegerEnum? EnumInteger, EnumNumberEnum? EnumNumber, OuterEnum OuterEnum)
        {
            
            this.EnumString = EnumString;
            
            this.EnumInteger = EnumInteger;
            
            this.EnumNumber = EnumNumber;
            
            this.OuterEnum = OuterEnum;
            
        }

        /// <summary>
        /// Returns builder of EnumTest.
        /// </summary>
        /// <returns>EnumTestBuilder</returns>
        public static EnumTestBuilder Builder()
        {
            return new EnumTestBuilder();
        }

        /// <summary>
        /// Returns EnumTestBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>EnumTestBuilder</returns>
        public EnumTestBuilder With()
        {
            return Builder()
                .EnumString(EnumString)
                .EnumInteger(EnumInteger)
                .EnumNumber(EnumNumber)
                .OuterEnum(OuterEnum);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(EnumTest other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (EnumTest.
        /// </summary>
        /// <param name="left">Compared (EnumTest</param>
        /// <param name="right">Compared (EnumTest</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (EnumTest left, EnumTest right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (EnumTest.
        /// </summary>
        /// <param name="left">Compared (EnumTest</param>
        /// <param name="right">Compared (EnumTest</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (EnumTest left, EnumTest right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of EnumTest.
        /// </summary>
        public sealed class EnumTestBuilder
        {
            private EnumStringEnum? _EnumString;
            private EnumIntegerEnum? _EnumInteger;
            private EnumNumberEnum? _EnumNumber;
            private OuterEnum _OuterEnum;

            internal EnumTestBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for EnumTest.EnumString property.
            /// </summary>
            /// <param name="value">EnumString</param>
            public EnumTestBuilder EnumString(EnumStringEnum? value)
            {
                _EnumString = value;
                return this;
            }

            /// <summary>
            /// Sets value for EnumTest.EnumInteger property.
            /// </summary>
            /// <param name="value">EnumInteger</param>
            public EnumTestBuilder EnumInteger(EnumIntegerEnum? value)
            {
                _EnumInteger = value;
                return this;
            }

            /// <summary>
            /// Sets value for EnumTest.EnumNumber property.
            /// </summary>
            /// <param name="value">EnumNumber</param>
            public EnumTestBuilder EnumNumber(EnumNumberEnum? value)
            {
                _EnumNumber = value;
                return this;
            }

            /// <summary>
            /// Sets value for EnumTest.OuterEnum property.
            /// </summary>
            /// <param name="value">OuterEnum</param>
            public EnumTestBuilder OuterEnum(OuterEnum value)
            {
                _OuterEnum = value;
                return this;
            }


            /// <summary>
            /// Builds instance of EnumTest.
            /// </summary>
            /// <returns>EnumTest</returns>
            public EnumTest Build()
            {
                Validate();
                return new EnumTest(
                    EnumString: _EnumString,
                    EnumInteger: _EnumInteger,
                    EnumNumber: _EnumNumber,
                    OuterEnum: _OuterEnum
                );
            }

            private void Validate()
            { 
            }
        }

        
        public enum EnumStringEnum { UPPER, Lower, Empty };
        public enum EnumIntegerEnum { _1, _1 };
        public enum EnumNumberEnum { _11, _12 };
    }
}

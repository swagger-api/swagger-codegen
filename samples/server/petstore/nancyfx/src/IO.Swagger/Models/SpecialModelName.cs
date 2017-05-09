using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// SpecialModelName
    /// </summary>
    public sealed class SpecialModelName:  IEquatable<SpecialModelName>
    { 
        /// <summary>
        /// SpecialPropertyName
        /// </summary>
        public long? SpecialPropertyName { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use SpecialModelName.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public SpecialModelName()
        {
        }

        private SpecialModelName(long? SpecialPropertyName)
        {
            
            this.SpecialPropertyName = SpecialPropertyName;
            
        }

        /// <summary>
        /// Returns builder of SpecialModelName.
        /// </summary>
        /// <returns>SpecialModelNameBuilder</returns>
        public static SpecialModelNameBuilder Builder()
        {
            return new SpecialModelNameBuilder();
        }

        /// <summary>
        /// Returns SpecialModelNameBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>SpecialModelNameBuilder</returns>
        public SpecialModelNameBuilder With()
        {
            return Builder()
                .SpecialPropertyName(SpecialPropertyName);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(SpecialModelName other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (SpecialModelName.
        /// </summary>
        /// <param name="left">Compared (SpecialModelName</param>
        /// <param name="right">Compared (SpecialModelName</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (SpecialModelName left, SpecialModelName right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (SpecialModelName.
        /// </summary>
        /// <param name="left">Compared (SpecialModelName</param>
        /// <param name="right">Compared (SpecialModelName</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (SpecialModelName left, SpecialModelName right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of SpecialModelName.
        /// </summary>
        public sealed class SpecialModelNameBuilder
        {
            private long? _SpecialPropertyName;

            internal SpecialModelNameBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for SpecialModelName.SpecialPropertyName property.
            /// </summary>
            /// <param name="value">SpecialPropertyName</param>
            public SpecialModelNameBuilder SpecialPropertyName(long? value)
            {
                _SpecialPropertyName = value;
                return this;
            }


            /// <summary>
            /// Builds instance of SpecialModelName.
            /// </summary>
            /// <returns>SpecialModelName</returns>
            public SpecialModelName Build()
            {
                Validate();
                return new SpecialModelName(
                    SpecialPropertyName: _SpecialPropertyName
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

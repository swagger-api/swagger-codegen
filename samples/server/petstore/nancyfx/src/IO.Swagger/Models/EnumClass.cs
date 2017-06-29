using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// EnumClass
    /// </summary>
    public sealed class EnumClass:  IEquatable<EnumClass>
    { 

        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use EnumClass.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public EnumClass()
        {
        }

        private EnumClass()
        {
            
        }

        /// <summary>
        /// Returns builder of EnumClass.
        /// </summary>
        /// <returns>EnumClassBuilder</returns>
        public static EnumClassBuilder Builder()
        {
            return new EnumClassBuilder();
        }

        /// <summary>
        /// Returns EnumClassBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>EnumClassBuilder</returns>
        public EnumClassBuilder With()
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

        public bool Equals(EnumClass other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (EnumClass.
        /// </summary>
        /// <param name="left">Compared (EnumClass</param>
        /// <param name="right">Compared (EnumClass</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (EnumClass left, EnumClass right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (EnumClass.
        /// </summary>
        /// <param name="left">Compared (EnumClass</param>
        /// <param name="right">Compared (EnumClass</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (EnumClass left, EnumClass right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of EnumClass.
        /// </summary>
        public sealed class EnumClassBuilder
        {

            internal EnumClassBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }


            /// <summary>
            /// Builds instance of EnumClass.
            /// </summary>
            /// <returns>EnumClass</returns>
            public EnumClass Build()
            {
                Validate();
                return new EnumClass(
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

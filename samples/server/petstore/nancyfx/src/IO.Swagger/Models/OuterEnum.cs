using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// OuterEnum
    /// </summary>
    public sealed class OuterEnum:  IEquatable<OuterEnum>
    { 

        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use OuterEnum.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public OuterEnum()
        {
        }

        private OuterEnum()
        {
            
        }

        /// <summary>
        /// Returns builder of OuterEnum.
        /// </summary>
        /// <returns>OuterEnumBuilder</returns>
        public static OuterEnumBuilder Builder()
        {
            return new OuterEnumBuilder();
        }

        /// <summary>
        /// Returns OuterEnumBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>OuterEnumBuilder</returns>
        public OuterEnumBuilder With()
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

        public bool Equals(OuterEnum other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (OuterEnum.
        /// </summary>
        /// <param name="left">Compared (OuterEnum</param>
        /// <param name="right">Compared (OuterEnum</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (OuterEnum left, OuterEnum right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (OuterEnum.
        /// </summary>
        /// <param name="left">Compared (OuterEnum</param>
        /// <param name="right">Compared (OuterEnum</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (OuterEnum left, OuterEnum right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of OuterEnum.
        /// </summary>
        public sealed class OuterEnumBuilder
        {

            internal OuterEnumBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }


            /// <summary>
            /// Builds instance of OuterEnum.
            /// </summary>
            /// <returns>OuterEnum</returns>
            public OuterEnum Build()
            {
                Validate();
                return new OuterEnum(
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

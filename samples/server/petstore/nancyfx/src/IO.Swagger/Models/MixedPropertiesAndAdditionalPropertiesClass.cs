using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// MixedPropertiesAndAdditionalPropertiesClass
    /// </summary>
    public sealed class MixedPropertiesAndAdditionalPropertiesClass:  IEquatable<MixedPropertiesAndAdditionalPropertiesClass>
    { 
        /// <summary>
        /// Uuid
        /// </summary>
        public Guid? Uuid { get; private set; }

        /// <summary>
        /// DateTime
        /// </summary>
        public ZonedDateTime? DateTime { get; private set; }

        /// <summary>
        /// Map
        /// </summary>
        public Dictionary<string, Animal> Map { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use MixedPropertiesAndAdditionalPropertiesClass.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public MixedPropertiesAndAdditionalPropertiesClass()
        {
        }

        private MixedPropertiesAndAdditionalPropertiesClass(Guid? Uuid, ZonedDateTime? DateTime, Dictionary<string, Animal> Map)
        {
            
            this.Uuid = Uuid;
            
            this.DateTime = DateTime;
            
            this.Map = Map;
            
        }

        /// <summary>
        /// Returns builder of MixedPropertiesAndAdditionalPropertiesClass.
        /// </summary>
        /// <returns>MixedPropertiesAndAdditionalPropertiesClassBuilder</returns>
        public static MixedPropertiesAndAdditionalPropertiesClassBuilder Builder()
        {
            return new MixedPropertiesAndAdditionalPropertiesClassBuilder();
        }

        /// <summary>
        /// Returns MixedPropertiesAndAdditionalPropertiesClassBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>MixedPropertiesAndAdditionalPropertiesClassBuilder</returns>
        public MixedPropertiesAndAdditionalPropertiesClassBuilder With()
        {
            return Builder()
                .Uuid(Uuid)
                .DateTime(DateTime)
                .Map(Map);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(MixedPropertiesAndAdditionalPropertiesClass other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (MixedPropertiesAndAdditionalPropertiesClass.
        /// </summary>
        /// <param name="left">Compared (MixedPropertiesAndAdditionalPropertiesClass</param>
        /// <param name="right">Compared (MixedPropertiesAndAdditionalPropertiesClass</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (MixedPropertiesAndAdditionalPropertiesClass left, MixedPropertiesAndAdditionalPropertiesClass right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (MixedPropertiesAndAdditionalPropertiesClass.
        /// </summary>
        /// <param name="left">Compared (MixedPropertiesAndAdditionalPropertiesClass</param>
        /// <param name="right">Compared (MixedPropertiesAndAdditionalPropertiesClass</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (MixedPropertiesAndAdditionalPropertiesClass left, MixedPropertiesAndAdditionalPropertiesClass right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of MixedPropertiesAndAdditionalPropertiesClass.
        /// </summary>
        public sealed class MixedPropertiesAndAdditionalPropertiesClassBuilder
        {
            private Guid? _Uuid;
            private ZonedDateTime? _DateTime;
            private Dictionary<string, Animal> _Map;

            internal MixedPropertiesAndAdditionalPropertiesClassBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for MixedPropertiesAndAdditionalPropertiesClass.Uuid property.
            /// </summary>
            /// <param name="value">Uuid</param>
            public MixedPropertiesAndAdditionalPropertiesClassBuilder Uuid(Guid? value)
            {
                _Uuid = value;
                return this;
            }

            /// <summary>
            /// Sets value for MixedPropertiesAndAdditionalPropertiesClass.DateTime property.
            /// </summary>
            /// <param name="value">DateTime</param>
            public MixedPropertiesAndAdditionalPropertiesClassBuilder DateTime(ZonedDateTime? value)
            {
                _DateTime = value;
                return this;
            }

            /// <summary>
            /// Sets value for MixedPropertiesAndAdditionalPropertiesClass.Map property.
            /// </summary>
            /// <param name="value">Map</param>
            public MixedPropertiesAndAdditionalPropertiesClassBuilder Map(Dictionary<string, Animal> value)
            {
                _Map = value;
                return this;
            }


            /// <summary>
            /// Builds instance of MixedPropertiesAndAdditionalPropertiesClass.
            /// </summary>
            /// <returns>MixedPropertiesAndAdditionalPropertiesClass</returns>
            public MixedPropertiesAndAdditionalPropertiesClass Build()
            {
                Validate();
                return new MixedPropertiesAndAdditionalPropertiesClass(
                    Uuid: _Uuid,
                    DateTime: _DateTime,
                    Map: _Map
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

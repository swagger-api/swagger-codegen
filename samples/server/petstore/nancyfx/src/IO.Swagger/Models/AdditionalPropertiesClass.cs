using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// AdditionalPropertiesClass
    /// </summary>
    public sealed class AdditionalPropertiesClass:  IEquatable<AdditionalPropertiesClass>
    { 
        /// <summary>
        /// MapProperty
        /// </summary>
        public Dictionary<string, string> MapProperty { get; private set; }

        /// <summary>
        /// MapOfMapProperty
        /// </summary>
        public Dictionary<string, Dictionary<string, string>> MapOfMapProperty { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use AdditionalPropertiesClass.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public AdditionalPropertiesClass()
        {
        }

        private AdditionalPropertiesClass(Dictionary<string, string> MapProperty, Dictionary<string, Dictionary<string, string>> MapOfMapProperty)
        {
            
            this.MapProperty = MapProperty;
            
            this.MapOfMapProperty = MapOfMapProperty;
            
        }

        /// <summary>
        /// Returns builder of AdditionalPropertiesClass.
        /// </summary>
        /// <returns>AdditionalPropertiesClassBuilder</returns>
        public static AdditionalPropertiesClassBuilder Builder()
        {
            return new AdditionalPropertiesClassBuilder();
        }

        /// <summary>
        /// Returns AdditionalPropertiesClassBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>AdditionalPropertiesClassBuilder</returns>
        public AdditionalPropertiesClassBuilder With()
        {
            return Builder()
                .MapProperty(MapProperty)
                .MapOfMapProperty(MapOfMapProperty);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(AdditionalPropertiesClass other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (AdditionalPropertiesClass.
        /// </summary>
        /// <param name="left">Compared (AdditionalPropertiesClass</param>
        /// <param name="right">Compared (AdditionalPropertiesClass</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (AdditionalPropertiesClass left, AdditionalPropertiesClass right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (AdditionalPropertiesClass.
        /// </summary>
        /// <param name="left">Compared (AdditionalPropertiesClass</param>
        /// <param name="right">Compared (AdditionalPropertiesClass</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (AdditionalPropertiesClass left, AdditionalPropertiesClass right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of AdditionalPropertiesClass.
        /// </summary>
        public sealed class AdditionalPropertiesClassBuilder
        {
            private Dictionary<string, string> _MapProperty;
            private Dictionary<string, Dictionary<string, string>> _MapOfMapProperty;

            internal AdditionalPropertiesClassBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for AdditionalPropertiesClass.MapProperty property.
            /// </summary>
            /// <param name="value">MapProperty</param>
            public AdditionalPropertiesClassBuilder MapProperty(Dictionary<string, string> value)
            {
                _MapProperty = value;
                return this;
            }

            /// <summary>
            /// Sets value for AdditionalPropertiesClass.MapOfMapProperty property.
            /// </summary>
            /// <param name="value">MapOfMapProperty</param>
            public AdditionalPropertiesClassBuilder MapOfMapProperty(Dictionary<string, Dictionary<string, string>> value)
            {
                _MapOfMapProperty = value;
                return this;
            }


            /// <summary>
            /// Builds instance of AdditionalPropertiesClass.
            /// </summary>
            /// <returns>AdditionalPropertiesClass</returns>
            public AdditionalPropertiesClass Build()
            {
                Validate();
                return new AdditionalPropertiesClass(
                    MapProperty: _MapProperty,
                    MapOfMapProperty: _MapOfMapProperty
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

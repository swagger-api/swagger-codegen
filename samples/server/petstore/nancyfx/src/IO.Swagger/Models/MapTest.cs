using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// MapTest
    /// </summary>
    public sealed class MapTest:  IEquatable<MapTest>
    { 
        /// <summary>
        /// MapMapOfString
        /// </summary>
        public Dictionary<string, Dictionary<string, string>> MapMapOfString { get; private set; }

        /// <summary>
        /// MapOfEnumString
        /// </summary>
        public Dictionary<string, InnerEnum>? MapOfEnumString { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use MapTest.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public MapTest()
        {
        }

        private MapTest(Dictionary<string, Dictionary<string, string>> MapMapOfString, Dictionary<string, InnerEnum>? MapOfEnumString)
        {
            
            this.MapMapOfString = MapMapOfString;
            
            this.MapOfEnumString = MapOfEnumString;
            
        }

        /// <summary>
        /// Returns builder of MapTest.
        /// </summary>
        /// <returns>MapTestBuilder</returns>
        public static MapTestBuilder Builder()
        {
            return new MapTestBuilder();
        }

        /// <summary>
        /// Returns MapTestBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>MapTestBuilder</returns>
        public MapTestBuilder With()
        {
            return Builder()
                .MapMapOfString(MapMapOfString)
                .MapOfEnumString(MapOfEnumString);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(MapTest other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (MapTest.
        /// </summary>
        /// <param name="left">Compared (MapTest</param>
        /// <param name="right">Compared (MapTest</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (MapTest left, MapTest right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (MapTest.
        /// </summary>
        /// <param name="left">Compared (MapTest</param>
        /// <param name="right">Compared (MapTest</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (MapTest left, MapTest right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of MapTest.
        /// </summary>
        public sealed class MapTestBuilder
        {
            private Dictionary<string, Dictionary<string, string>> _MapMapOfString;
            private Dictionary<string, InnerEnum>? _MapOfEnumString;

            internal MapTestBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for MapTest.MapMapOfString property.
            /// </summary>
            /// <param name="value">MapMapOfString</param>
            public MapTestBuilder MapMapOfString(Dictionary<string, Dictionary<string, string>> value)
            {
                _MapMapOfString = value;
                return this;
            }

            /// <summary>
            /// Sets value for MapTest.MapOfEnumString property.
            /// </summary>
            /// <param name="value">MapOfEnumString</param>
            public MapTestBuilder MapOfEnumString(Dictionary<string, InnerEnum>? value)
            {
                _MapOfEnumString = value;
                return this;
            }


            /// <summary>
            /// Builds instance of MapTest.
            /// </summary>
            /// <returns>MapTest</returns>
            public MapTest Build()
            {
                Validate();
                return new MapTest(
                    MapMapOfString: _MapMapOfString,
                    MapOfEnumString: _MapOfEnumString
                );
            }

            private void Validate()
            { 
            }
        }

        
        public enum Dictionary&lt;string, InnerEnum&gt; { UPPER, Lower };
        public enum InnerEnum { UPPER, Lower };
    }
}

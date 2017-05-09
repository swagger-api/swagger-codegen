using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// List
    /// </summary>
    public sealed class List:  IEquatable<List>
    { 
        /// <summary>
        /// _123List
        /// </summary>
        public string _123List { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use List.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public List()
        {
        }

        private List(string _123List)
        {
            
            this._123List = _123List;
            
        }

        /// <summary>
        /// Returns builder of List.
        /// </summary>
        /// <returns>ListBuilder</returns>
        public static ListBuilder Builder()
        {
            return new ListBuilder();
        }

        /// <summary>
        /// Returns ListBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ListBuilder</returns>
        public ListBuilder With()
        {
            return Builder()
                ._123List(_123List);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(List other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (List.
        /// </summary>
        /// <param name="left">Compared (List</param>
        /// <param name="right">Compared (List</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (List left, List right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (List.
        /// </summary>
        /// <param name="left">Compared (List</param>
        /// <param name="right">Compared (List</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (List left, List right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of List.
        /// </summary>
        public sealed class ListBuilder
        {
            private string __123List;

            internal ListBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for List._123List property.
            /// </summary>
            /// <param name="value">_123List</param>
            public ListBuilder _123List(string value)
            {
                __123List = value;
                return this;
            }


            /// <summary>
            /// Builds instance of List.
            /// </summary>
            /// <returns>List</returns>
            public List Build()
            {
                Validate();
                return new List(
                    _123List: __123List
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}

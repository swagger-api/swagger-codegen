using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// EnumArrays
    /// </summary>
    public sealed class EnumArrays:  IEquatable<EnumArrays>
    { 
        /// <summary>
        /// JustSymbol
        /// </summary>
        public JustSymbolEnum? JustSymbol { get; private set; }

        /// <summary>
        /// ArrayEnum
        /// </summary>
        public List<ArrayEnumEnum>? ArrayEnum { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use EnumArrays.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public EnumArrays()
        {
        }

        private EnumArrays(JustSymbolEnum? JustSymbol, List<ArrayEnumEnum>? ArrayEnum)
        {
            
            this.JustSymbol = JustSymbol;
            
            this.ArrayEnum = ArrayEnum;
            
        }

        /// <summary>
        /// Returns builder of EnumArrays.
        /// </summary>
        /// <returns>EnumArraysBuilder</returns>
        public static EnumArraysBuilder Builder()
        {
            return new EnumArraysBuilder();
        }

        /// <summary>
        /// Returns EnumArraysBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>EnumArraysBuilder</returns>
        public EnumArraysBuilder With()
        {
            return Builder()
                .JustSymbol(JustSymbol)
                .ArrayEnum(ArrayEnum);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(EnumArrays other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (EnumArrays.
        /// </summary>
        /// <param name="left">Compared (EnumArrays</param>
        /// <param name="right">Compared (EnumArrays</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (EnumArrays left, EnumArrays right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (EnumArrays.
        /// </summary>
        /// <param name="left">Compared (EnumArrays</param>
        /// <param name="right">Compared (EnumArrays</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (EnumArrays left, EnumArrays right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of EnumArrays.
        /// </summary>
        public sealed class EnumArraysBuilder
        {
            private JustSymbolEnum? _JustSymbol;
            private List<ArrayEnumEnum>? _ArrayEnum;

            internal EnumArraysBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for EnumArrays.JustSymbol property.
            /// </summary>
            /// <param name="value">JustSymbol</param>
            public EnumArraysBuilder JustSymbol(JustSymbolEnum? value)
            {
                _JustSymbol = value;
                return this;
            }

            /// <summary>
            /// Sets value for EnumArrays.ArrayEnum property.
            /// </summary>
            /// <param name="value">ArrayEnum</param>
            public EnumArraysBuilder ArrayEnum(List<ArrayEnumEnum>? value)
            {
                _ArrayEnum = value;
                return this;
            }


            /// <summary>
            /// Builds instance of EnumArrays.
            /// </summary>
            /// <returns>EnumArrays</returns>
            public EnumArrays Build()
            {
                Validate();
                return new EnumArrays(
                    JustSymbol: _JustSymbol,
                    ArrayEnum: _ArrayEnum
                );
            }

            private void Validate()
            { 
            }
        }

        
        public enum JustSymbolEnum { , Value };
        public enum List&lt;ArrayEnumEnum&gt; { Fish, Crab };
        public enum ArrayEnumEnum { Fish, Crab };
    }
}
